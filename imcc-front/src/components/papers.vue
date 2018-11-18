<template>
  <!-- <keywords></keywords> -->
  <v-container id="filter">
    <v-card with="800">
      <v-flex mx-4 my-5 px-1 py-5>
        <h1>文章检索</h1>
        <h3>使用过滤器或直接点击检索</h3>
        <v-form ref="form">
          <v-select :items="categories" item-text="name" v-model="category" return-object label="版块"/>
          <v-select :items="years" v-model="year" label="年份"/>
          <v-text-field v-model="auther" :rules="autherRules" :counter="autherLen"
            label="作者" />
          <keywords
            :keywordList="keywords"
            ref="keywords"
          />
          <v-btn large color="primary" @click="search" >
            检索
          </v-btn>
          </v-form>
      </v-flex>
    </v-card>

    <!-- paper list -->
    <v-card v-for="(paper, i) in papers">
      <h2>{{paper.title}}</h2>
      <v-layout>
        <h3>{{paper.auther}}</h3>
        <h3>{{paper.year}}-{{paper.month}}-{{paper.day}}</h3>
        <h3 v-for="(k, i) in paper.keywords">{{k.name}}</h3>
      </v-layout>
    </v-card>
  </v-container>
</template>

<script>
import keywords from './keywords'
import {imccGlobal} from '../lib/global.js'
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
        keywords : [],
        papers: [],
    }
  },
  mounted() {
    console.log(imccGlobal)
  },
  methods: {
    search() {
      if (this.$refs.form.validate()) {
        let option = {}
        if (this.category) option.category = this.category.name
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
.h3, .h4 {
  text-align: center;
}
</style>
