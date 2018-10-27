<template>
  <v-container id="upload">
    <v-card class="form" with="800">
      <v-flex mx-4 my-5 px-1 py-5>
        <v-form v-model="valid">
          <v-text-field v-model="title" :rules="titleRules" :counter="titleLen"
            label="标题" required />
          <v-textarea v-model="abstract" :rules="abstractRules" :counter="abstractLen"
            label="摘要" required/>
          <v-layout>
            <v-text-field v-for="(keyword, i) in keywords"
              :key="i"
              v-model="keyword.text" :rules="keywordsRules" :counter="keywordLen"
              @blur="OnKeywordBlur(i)"
              label="关键字" required/>
          </v-layout>
          <upload-btn large color="black" title="" loading :fileChangedCallback="OnFileChanged">
            <template slot="icon">
              <v-icon dark >{{upIcon}}</v-icon>
            </template>
          </upload-btn>
        </v-form>
      </v-flex>
    </v-card>
  </v-container>
</template>

<script>
import UploadButton from 'vuetify-upload-button';
export default {
  data() {
    return {
      uploaded : false,
      upIcon: 'fas fa-file-upload',
      valid: false,
      title: '',
      titleLen: 50,
      titleRules: [
        v => !!v || '标题不能为空',
        v => v.length <= this.titleLen || '标题最多50字',
      ],
      abstract: '',
      abstractLen: 500,
      abstractRules: [
        v => !!v || '摘要不能为空',
        v => v.length <= this.abstractLen || '摘要最多500字',
      ],
      keywordLen: 4,
      maxKeywords: 5,
      keywords : [],
      keywordsRules: [
        v => v.length <= this.keywordLen || '关键字最多4字',
      ],
    }
  },
  computed: {
    numFilledKeyword: function() {
      let result = 0;
      for (let i = 0; i < this.keywords.length; i++) {
        if (this.keywords[i].text.length > 0) {
          result++;
        }
      }
      return result;
    }
  },
  mounted() {
    this.addKeywordInput(0, true);
  },
  methods: {
    OnFileChanged(file) {
      console.log(file)
    },
    addKeywordInput(i, force) {
      if (!force && i > this.keywords.length) {
        return
      }
      this.keywords.push({
        text: '',
      })
    },
    OnKeywordBlur(i) {
      let keyword = this.keywords[i]
      if (keyword.text.length > 0) {
        if (i == this.keywords.length - 1) {
          this.addKeywordInput(i + 1)
        }
      }
      else {
        let toDel = -1;
        for (let j = i; j < this.keywords.length; j++) {
          if (j >= this.keywords.length - 1) {
            toDel = j;
          }
          else {
            this.keywords[j].text = this.keywords[j+1].text
          }
        }
        this.keywords.splice(toDel, 1)
      }
    }
  },
  components: {
    'upload-btn': UploadButton,
  }
}
</script>

<style lang="css">
</style>
