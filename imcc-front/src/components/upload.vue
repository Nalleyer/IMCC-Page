<template>
  <v-container id="upload">
    <v-card class="form" with="800">
      <v-flex mx-4 my-5 px-1 py-5>
        <v-form method="post" action="/api/upload" ref="form" v-model="valid">
          <v-select :items="categories" item-text="name" v-model="category" return-object label="请选择版块"/>
          <v-text-field v-model="title" :rules="titleRules" :counter="titleLen"
            label="标题" required />
          <v-text-field v-model="auther" :rules="autherRules" :counter="autherLen"
            label="作者" required />
          <v-textarea v-model="abstract" :rules="abstractRules" :counter="abstractLen"
            label="摘要" required/>
          <keywords
            :keywordList="keywords"
          >
          </keywords>
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
import keywords from './keywords';
import {imccGlobal} from '../lib/global.js'
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
      auther: '',
      autherLen: 10,
      autherRules: [
        v => !!v || '作者不能为空',
        v => v.length <= this.autherLen || '作者名最多10字',
      ],
      abstract: '',
      abstractLen: 500,
      abstractRules: [
        v => !!v || '摘要不能为空',
        v => v.length <= this.abstractLen || '摘要最多500字',
      ],
      keywords : [],
      fileInfo: null,
      maxFileMb: 50,
      category: null,
      categories: imccGlobal.categories,
    }
  },
  computed: {
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
  methods: {
    OnFileChanged(file) {
      console.log(file)
      this.fileInfo = file
    },
    submit() {
      let ok = this.$refs.form.validate() && this.fileOk
      if (!ok) {
        console.log("no")
        return
      }
      let formData = new FormData()
      formData.append("title", this.title)
      formData.append("auther", this.auther)
      formData.append("abstract", this.abstract)
      formData.append("keywords", this.keywordsFormData)
      formData.append("file", this.fileInfo)
      formData.append("category", this.category.name)
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
    'keywords': keywords,
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
