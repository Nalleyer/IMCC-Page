<template lang="html">
  <div>
    <div class="main-section">
      <video-bg :sources="[vbg]" :img="bg">
      </video-bg>
    </div>
    <div class="main-section">IMCC Oh yeah</div>
    <div class="main-section">2</div>
    <div class="main-section">3</div>
    <div class="main-section">4</div>
    <div class="main-section">5</div>
  </div>

</template>

<script>
import VideoBg from 'vue-videobg'

export default {
  data() {
    return {
      bg: require("@/assets/imcc_fog.png"),
      vbg : require("@/assets/imcc_fog.mp4"),
    }
  },
  created() {
  },
  mounted() {
  },
  methods: {
  },
  components: {
    VideoBg,
  }
}
</script>

<style lang="css">

.main-section {
  height: 800px;
}

video {
  filter: brightness(60%);
}

</style>
