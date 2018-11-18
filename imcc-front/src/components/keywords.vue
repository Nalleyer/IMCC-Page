<template>
  <v-layout>
    <v-text-field v-for="(keyword, i) in keywordList"
    :key="i"
    v-model="keyword.text" :rules="keywordsRules" :counter="strLen"
    @blur="OnKeywordBlur(i)"
    :label="label" required/>
  </v-layout>
</template>

<script>
export default {
  name: "keywords",
  props: {
      label: {
        type: String,
        default: "关键字",
      },
      strLen: {
        type: Number,
        default: 4,
      },
      maxCount: {
        type: Number,
        default: 5,
      },
      keywordsRules: {
        type: Array,
        default: function() { return [
          v => v.length <= this.strLen || this.label + '最多4字',
          v => /^[^; ]*$/.test(v) || this.label + '请不要包含英文分号 \';\' 或空格',
        ]}
      },
      keywordList: {
        type: Array,
        default: function() {
          return []
        }
      },
  },
  data() {
    return {
    }
  },
  computed: {
    numFilledKeyword: function() {
      let result = 0
      for (let i = 0; i < this.keywordList.length; i++) {
        if (this.keywordList[i].text.length > 0) {
          result++
        }
      }
      return result
    },
    joinedKeywords: function() {
      return this.keywordList.map(x => x.text).filter(x => x != "").join(';')
    },
  },
  mounted() {
    this.addKeywordInput(0, true)
  },
  methods: {
    addKeywordInput(i, force) {
      if (!force && i > this.keywordList.length) {
        return
      }
      this.keywordList.push({
        text: '',
      })
    },
    OnKeywordBlur(i) {
      let keyword = this.keywordList[i]
      if (keyword.text.length > 0) {
        if (this.keywordList.length < this.maxCount && i == this.keywordList.length - 1) {
          this.addKeywordInput(i + 1)
        }
      }
      else if (this.keywordList.length > 1) {
        let toDel = -1
        for (let j = i; j < this.keywordList.length; j++) {
          if (j >= this.keywordList.length - 1) {
            toDel = j
          }
          else {
            this.keywordList[j].text = this.keywordList[j+1].text
          }
        }
        let len = this.keywordList.length
        if (len == this.maxKeywords && toDel == len - 1) {
          this.keywordList[len - 1].text = ""
        }
        else {
          this.keywordList.splice(toDel, 1)
        }
      }
    },
  }
}
</script>

<style>
</style>
