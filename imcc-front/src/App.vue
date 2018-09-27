<template>
  <div id="app">
    <v-app id="inside">
      <mheader></mheader>
      <v-content>
        <router-view id="main"> oh no </router-view>
      </v-content>
    </v-app>
  </div>
</template>

<script>
import mheader from "@/components/mheader"

export default {
  name: 'app',
  components: {
    mheader
  },
  data () {
    return {
      msg: 'Welcome to Your Vue.js App',
    }
  },
  methods: {
    upload() {
      this.$router.push("/upload")
    },
    back() {
      this.$router.back()
    }
  }
}
</script>

<style>
@import url('https://fonts.googleapis.com/css?family=Monoton');
#app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  /* margin-top: 60px; */
}

body {
  margin: 0px;
}

#main {
  margin-top: -64px;
}
</style>
