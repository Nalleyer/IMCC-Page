<template>
<!-- <keywords></keywords> -->
<v-container id="filter">
  <v-card with="800">
    <v-flex mx-4 my-5 px-1 py-5>
      <h1>文章检索</h1>
      <h3>使用过滤器或直接点击检索</h3>
      <v-form ref="form">
        <v-select :items="categories" item-text="name" v-model="category" return-object label="版块" />
        <v-select :items="years" v-model="year" label="年份" />
        <v-text-field v-model="auther" :rules="autherRules" :counter="autherLen" label="作者" />
        <keywords :keywordList="keywords" ref="keywords" />
        <v-btn large color="primary" @click="search">
          检索
        </v-btn>
      </v-form>
    </v-flex>
  </v-card>

  <!-- paper list -->
  <v-container fluid grid-list-sm>
    <v-layout row wrap>
      <v-flex v-for="(paper, i) in papers" :key="paper.pId" xs4>
        <v-card>
          <v-flex mx-3 my-3 px-1 py-3>
            <p class="paper-title" title="标题">{{paper.title}}</p>
            <v-divider light></v-divider>
            <v-card-title>
              <h3 title="作者" class="pr2"> 作者：{{paper.auther}}</h3>
              <h3 title="投稿日期" class="pr2"> 投稿日期：{{paper.year}}-{{paper.month}}-{{paper.day}} </h3>
              <h3 title="版块" class="pr2"> 版块：{{paper.categoryName}} </h3>
            </v-card-title>
            <v-divider light></v-divider>
            <v-card-title>
              <v-layout>
                <v-flex>
                  <v-layout>
                    <h4>关键字：</h4>
                    <div class="pr1" v-for="(k, j) in paper.keywords">
                      <h4 v-if="j < maxShowKeywords">{{k}}</h4>
                    </div>
                    <h4 v-if="paper.keywords.length > maxShowKeywords">...</h4>
                  </v-layout>
                </v-flex>
                <v-btn title="下载全文" class="mx-3 white--text" color="secondary" icon dark large @click="OnDownloadClick(paper)">
                  <v-icon size="30px">{{dlIcon}}</v-icon>
                </v-btn>
                <v-btn title="在线预览" class="mx-3 white--text" color="secondary" icon dark large @click="OnPreviewClick(paper)">
                  <v-icon size="30px">{{pvIcon}}</v-icon>
                </v-btn>
              </v-layout>
            </v-card-title>
          </v-flex>
        </v-card>
      </v-flex>
    </v-layout>
  </v-container>
</v-container>
</template>

<script>
import keywords from './keywords'
import {
  imccGlobal
} from '../lib/global.js'
export default {
  components: {
    keywords,
  },
  data() {
    return {
      categories: imccGlobal.categories,
      years: [
        2018,
      ],
      category: null,
      year: null,
      auther: '',
      autherLen: 10,
      autherRules: [
        v => v.length <= this.autherLen || '作者名最多10字',
      ],
      keywords: [],
      maxShowKeywords: 4,
      dlIcon: 'fas fa-file-download',
      pvIcon: 'fas fa-eye',
      // dlIcon: 'fas fa-lock',
      // papers: [],
      // fake
      papers: [{
          abstract: "abstract",
          auther: "677",
          categoryId: 1,
          categoryName: "c1",
          day: 14,
          keywords: ['k1', 'k2', 'k3'],
          month: 11,
          year: 2018,
          title: 'title is me',
          pId: 1,
        },
        {
          abstract: "abstract",
          auther: "677",
          categoryId: 2,
          categoryName: "c2",
          day: 14,
          keywords: ['k1', 'k2', 'k3'],
          month: 11,
          year: 2018,
          title: 'title is me',
          pId: 2,
        },
        {
          abstract: "abstract 3",
          auther: "677",
          categoryId: 3,
          categoryName: "c3",
          day: 14,
          keywords: ['k1', 'k2', 'k3'],
          month: 11,
          year: 2018,
          title: 'title is me 3',
          pId: 3,
        },
        {
          abstract: "abstract",
          auther: "677",
          categoryId: 4,
          categoryName: "c4",
          day: 14,
          keywords: ['k1', 'k2', 'k3'],
          month: 11,
          year: 2018,
          title: 'title is me 4',
          pId: 4,
        },
      ],
    }
  },
  mounted() {
    console.log(imccGlobal)
  },
  methods: {
    search() {
      if (this.$refs.form.validate()) {
        let option = {}
        if (this.category) {
          console.log("ok")
          option.category = this.category.name
        }
        if (this.year) option.year = this.year
        if (this.auther) option.auther = this.auther
        if (this.$refs.keywords.joinedKeywords) option.keywords = this.$refs.keywords.joinedKeywords
        console.log(option)
        this.$http.get('/api/list/', {
          params: option,
        }).then(
          (res) => {
            this.papers = res.body;
            console.log(this.papers)
          },
          (res) => {
            console.log(res)
          }
        )
      }
    }
  }
}
</script>

<style>
.h3,
.h4 {
  text-align: center;
  font-family: 'Noto Sans', "微软雅黑", sans-serif;
}

.paper-title {
  text-align: center;
  font-size: 25px;
  font-family: 'Noto Sans', "微软雅黑", sans-serif;
}

.pr1 {
  padding-right: 12px;
}

.pr2 {
  padding-right: 25px;
}
</style>
