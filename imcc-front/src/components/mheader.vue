<template>
  <v-toolbar dark tabs height="64" app fixed
    v-scroll="onScroll"
    :class="{'transparent': isOnTopTForColor}"
    :style="style"
  >
    <v-tabs height="64" fixed-tabs
      color="transparent"
    >
    <div class="header-title" :class="{'top': isOnTopT}">IMCC</div>
      <v-spacer></v-spacer>
      <v-tab
      v-for="(item, index) in items"
      :key="index"
      width="80"
      :to="item.router"
      :class="{'tab-transparent': isOnTopTForColor}"
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
      ],
      topT: 800,
      awayT: 1600,
      offsetTopOld: null,
      offsetTop: 0,
      style: "transform: translateY(0px)",
    }
  },
  computed: {
    awayFromTop: function() {
      return this.offsetTop > this.awayT
    },
    scrollingDown: function() {
      return this.offsetTop > this.offsetTopOld
    },
    isOnTopT: function() {
      return this.offsetTop < this.topT
    },
    isOnTopTForColor: function() {
      return this.offsetTop < this.topT + 300
    },
    isOnTop: function() {
      return this.offsetTop < 1
    },
  },
  methods: {
    onScroll(e) {
      this.offsetTopOld = this.offsetTop
      this.offsetTop = window.pageYOffset || document.documentElement.scrollTop

      if (!this.scrollingDown) {
        this.showBar()
      }
      else if (this.isOnTopT) {
        this.showBar()
      }
      else {
        this.hideBar()
      }
    },
    showBar() {
      this.style = 'transform: translateY(0px)'
    },
    hideBar() {
      this.style = 'transform: translateY(-64px)'
    },
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

.top {
  color: #999;
}

.tab-transparent > a {
  color: #333;
}

</style>
