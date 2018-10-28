<template>
  <v-container id="upload">
    <v-card class="form" with="800">
      <v-flex mx-4 my-5 px-1 py-5>
        <v-form method="post" action="/api/upload" ref="form" v-model="valid">
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
          <v-layout>
          <h2>上传论文文件（PDF格式）：</h2>
          <upload-btn large color="black" title=""
            :fileChangedCallback="OnFileChanged"
            accept="application/pdf">
            <template slot="icon">
              <v-icon dark >{{upIcon}}</v-icon>
            </template>
          </upload-btn>
          <v-layout column justify-start>
            <h3 class="left-align" v-if="fileInfo != null"> {{ fileInfo.name }} </h3>
            <h3 class="left-align error-text"> {{ fileOkText }} </h3>
          </v-layout>
          </v-layout>
          <v-btn large color="primary" @click="submit" >
            投稿
          </v-btn>
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
        v => /^[^; ]*$/.test(v) || '关键字请不要包含英文分号 \';\' 或空格',
      ],
      fileInfo: null,
      maxFileMb: 50,
    }
  },
  computed: {
    numFilledKeyword: function() {
      let result = 0
      for (let i = 0; i < this.keywords.length; i++) {
        if (this.keywords[i].text.length > 0) {
          result++
        }
      }
      return result
    },
    fileOkText: function() {
      if (this.fileInfo == null) {
        return "未上传文件"
      }
      else if (this.fileInfo.size > this.maxFileMb << 20) {
        return "请将文件大小限制在" + this.maxFileMb + "MB以内……"
      }
      else {
        return ""
      }
    },
    fileOk: function() {
      // use "" as valid flag
      return this.fileOkText == ""
    },
    keywordsFormData: function() {
      return this.keywords.map(x => x.text).filter(x => x != "").join(';')
    },
  },
  mounted() {
    this.addKeywordInput(0, true)
  },
  methods: {
    OnFileChanged(file) {
      console.log(file)
      this.fileInfo = file
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
        if (this.keywords.length < this.maxKeywords && i == this.keywords.length - 1) {
          this.addKeywordInput(i + 1)
        }
      }
      else if (this.keywords.length > 1) {
        let toDel = -1
        for (let j = i; j < this.keywords.length; j++) {
          if (j >= this.keywords.length - 1) {
            toDel = j
          }
          else {
            this.keywords[j].text = this.keywords[j+1].text
          }
        }
        let len = this.keywords.length
        if (len == this.maxKeywords && toDel == len - 1) {
          this.keywords[len - 1].text = ""
        }
        else {
          this.keywords.splice(toDel, 1)
        }
      }
    },
    submit() {
      let ok = this.$refs.form.validate() && this.fileOk
      if (!ok) {
        console.log("no")
        return
      }
      let formData = new FormData()
      formData.append("title", this.title)
      formData.append("abstract", this.abstract)
      formData.append("keywords", this.keywordsFormData)
      formData.append("file", this.fileInfo)
      this.$http.post('/api/upload', formData, {
        'Content-Type': 'Multipart/form-data',
      }).then(resp => {
          console.log("submitted")
      }, resp => {
          console.log("failed")
      })
    }
  },
  components: {
    'upload-btn': UploadButton,
  }
}
</script>

<style lang="css">
.left-align {
  text-align: left;
}
.error-text {
  color: red;
}
</style>
