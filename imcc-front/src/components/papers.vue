<template>
  <!-- <keywords></keywords> -->
  <v-container id="filter">
    <v-card with="800">
      <v-flex mx-4 my-5 px-1 py-5>
        <h1>文章检索</h1>
        <v-select :items="categories" item-text="name" v-model="category" return-object label="版块"/>
      </v-flex>
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
        category: null,
    }
  },
  mounted() {
    console.log(imccGlobal)
  }
}
</script>

<style>
.h3, .h4 {
  text-align: center;
}
</style>
