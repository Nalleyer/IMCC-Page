<template>
  <v-toolbar dark tabs height="64" app fixed
    scroll-off-screen
    scroll-threshold="0"
    >
    <v-tabs height="64" fixed-tabs dark>
      <div class="header-title">IMCC</div>
      <v-spacer></v-spacer>
      <v-tab
      v-for="(item, index) in items"
      :key="index"
      width="80"
      :to="item.router"
      >
      {{ item.name }}
    </v-tab>
  </v-tabs>
</v-toolbar>
</template>

<script>
export default {
  data() {
    return {
      activeIndex: "1",
      items: [
        {
          name: "首页",
          router: "/main",
        },
        {
          name: "文章",
          router: "/papers",
        },
        {
          name: "投稿",
          router: "/submit",
        },
      ]
    }
  },
  methods: {
  }
}
</script>

<style lang="css">
.header-title {
  text-align: left;
  font-size: 36px;
  color:  #fff;
  font-family: 'Monoton', cursive;
  margin-left: 10px;
  padding-top: 6px;
}
.v-toolbar__content {
  padding-left: 0px;
  padding-right: 0px;
}

</style>
