<template>
  <div id="app">
    <v-app id="inside">
      <mheader></mheader>
      <v-content :class="{ 'to-top': isMain }">
        <router-view> oh no </router-view>
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
  mounted() {
    this.$router.afterEach((to, from, next) => {
    })
  },
  data () {
    return {
      msg: 'Welcome to Your Vue.js App',
    }
  },
  computed: {
    isMain: function() {
      return this.$route.path == "/main"
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
@import url('https://fonts.googleapis.com/css?family=Lato:100|Monoton|Muli|Noto+Sans|Open+Sans:300');
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

.to-top {
  margin-top: -64px;
}
</style>
