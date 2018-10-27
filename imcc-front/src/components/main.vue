<template lang="html">
  <div>
    <div>
      <video-bg class="parent" :sources="[vbg]" :img="bg">
        <v-container class="full-height">
          <v-layout align-end justify-end class="full-height">
            <v-card-text>
              <h1 class="text-xs-right title-1">International</h1>
              <h1 class="text-xs-right title-2">MineCraft Conference</h1>
            </v-card-text>
          </v-layout>
        </v-container>
      </video-bg>
    </div>

    <v-container my-5 class="main-section">
      <v-layout align-center column>
        <v-flex py-3 my-2> <h1 class="title-h1">国际MC会议</h1> </v-flex>
        <v-card-text> <h2 class="title-h2">
          <strong>国际MC会议</strong>是林克斯大陆首个国际会议，也是目前最专业的国际会议。 <br>
        </h2> </v-card-text>
      </v-layout>
    </v-container>

    <v-container px-0 my-5 class="main-section">
      <v-flex py-5 my-3> <h2 class="title-h2">会议主要分为3大版块</h2> </v-flex>
      <v-layout align-center justify-center>
        <v-layout column align-center>
          <v-img :src="redstoneBg" :width="cardImgSize" :height="cardImgSize"></v-img>
          <v-card-text> <h2 class="title-h2"> 科学技术 </h2> </v-card-text>
          <h3 class="card-text">
            科学技术是第一生产力，为激励科技发展，设立此版块，
            收录自主科研成果，顶尖科技调研综述，代码层机制研究等一切与科技相关内容。
          </h3>
        </v-layout>

        <v-layout column align-center>
          <v-img :src="treeBg" :width="cardImgSize" :height="cardImgSize"></v-img>
          <v-card-text> <h2 class="title-h2"> 建筑艺术 </h2> </v-card-text>
          <h3 class="card-text">
            建筑是人类生产实践中不可避免的产物。
            此版块欢迎关于建筑美学研究，建筑设计或成果展示，建筑方法论等一切与建筑有关的内容。
          </h3>
        </v-layout>
        <v-layout column align-center>
          <v-img :src="tcBg" :width="cardImgSize" :height="cardImgSize"></v-img>
          <v-card-text> <h2 class="title-h2"> 周边创作 </h2> </v-card-text>
          <h3 class="card-text">
            繁忙的游戏进程让你感到乏味了吗？试试创作点什么吧。
            本版块收录各种游戏相关或玩家相关的游戏外创作，人物传记，宣传视频，同人绘画……来者不拒。
          </h3>
        </v-layout>
      </v-layout>
    </v-container>

    <div id="newhome-bg"
      :style="{ 'background-image': 'url(' + require('@/assets/newhome.jpg') + ')', 'background-size': 'cover' }">
      <v-container mx-0 px-0 class="main-section full-height">
        <v-layout align-start justify-start class="full-height">
          <v-layout mt-5 align-start column>
            <v-flex py-3 my-2 ml-5 > <h1 class="title-1">IMCC 2018</h1> </v-flex>
            <v-flex ml-5> <h2 class="title-h2 left white-text">
              将于<br>12月25日在<br>新家市圣诞路0号 举行 <br><br>
              <small>欢迎各界人士积极参与</small>
            </h2> </v-flex>
          </v-layout>
        </v-layout>
      </v-container>
    </div>
    <!-- </v-img> -->

    <v-container my-5>

    </v-container>

    <v-footer height="auto">
      <v-card flat tile class="grey darken-3 white--text text-xs-center full-width">
        <v-card-text>
          <v-btn
          v-for="icon in icons"
          :key="icon"
          class="mx-3 white--text"
          icon
          large
          @click="OnFooterBtnClick(icon)"
          :href="icon.link"
          >
          <v-icon size="30px">{{ icon.asset }}</v-icon>
        </v-btn>
      </v-card-text>

      <v-card-text class="white--text pt-0">
        不知道为什么下面写一些字就会显得网页更专业一些，所以我也写了一些字在这里。
      </v-card-text>

      <v-divider></v-divider>

      <v-card-text class="white--text">
        &copy;2018 — <strong>Nalley</strong>
      </v-card-text>
    </v-card>
  </v-footer>
</div>

</template>

<script>
import VideoBg from 'vue-videobg'

export default {
  data() {
    return {
      bg: require("@/assets/imcc_fog.jpg"),
      vbg: require("@/assets/imcc_fog.mp4"),
      redstoneBg: require("@/assets/redstone.jpg"),
      treeBg: require("@/assets/tree.jpg"),
      tcBg: require("@/assets/tc.jpg"),
      newhomeBg: require("@/assets/newhome.jpg"),

      cardImgSize: 400,
      icons: [
        {
            asset: 'fab fa-google-plus',
            id: 0,
            link: "https://plus.google.com/u/0/+NalleyerInoru",
        },
        {
            asset: 'far fa-envelope',
            id: 1,
            link: "mailto:nalleyer@gmail.com",
        },
        {
            asset: 'fab fa-mastodon',
            id: 2,
            link: "https://pawoo.net/web/accounts/191271",
        },
      ],
    }
  },
  created() {
  },
  mounted() {
  },
  methods: {
    OnFooterBtnClick(icon) {
      console.log(icon.link)
    }
  },
  components: {
    VideoBg,
  }
}
</script>

<style lang="css">

.main-section {
  /* height: 800px; */
}

.sec-bg {
  background-color: #aaaaaa;
}

video {
  filter: brightness(60%);
}

.dim {
  filter: brightness(60%);
}

.parent {
  display: flex;
  flex-wrap: wrap;
}

.full-height {
  height: 100%;
}

.full-width {
  width: 100%;
}

.title-h1 {
  font-size: 48px;
  font-family: 'Noto Sans', "微软雅黑", sans-serif;
}

.title-h2 {
  font-size: 28px;
  font-family: 'Lato', "华文细黑", sans-serif;
  margin: 16px;
}

.white-text {
  color: #fff;
}

.left {
  margin-left: 0px;
  text-align: left;
}

.title-1 {
  font-size: 60px;
  color: #fff;
  font-family: 'Lato', sans-serif;
}

.title-2 {
  color: #ccc;
  font-size: 48px;
  font-family: 'Lato', sans-serif;
}

.h300 {
  height: 300px;
}

.card-text {
  width: 400px;
  word-wrap: break-word;
  line-height: 1.8;
}

#newhome-bg {
  width: 100%;
  height: 1080px;
}

</style>
