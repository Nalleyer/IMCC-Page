<template>
  <el-container>
    <el-upload
    ref="upload"
    action="/upload/test"
    :multiple="false"
    :file-list="fileList"
    accept="application/pdf"
    :on-success="onPdfSuccess"
    :before-upload="beforePdfUpload"
    :on-error="onPdfError"
    :auto-upload="false"
    drag
    >
    <i class="el-icon-upload"></i>
    <div class="el-upload__text">将文件拖到此处，或<em>点击选取文件</em></div>
  </el-upload>
  <el-button style="margin-left: 10px;" size="small" type="success" @click="submitUpload">上传</el-button>
  <h1 v-if="uploaded">ok</h1>
</el-container>
</template>

<script>
export default {
  data() {
    return {
      uploaded : false,
    }
  },
  methods: {
    onPdfSuccess(res, file) {
      console.log(res);
      console.log(file);
      this.uploaded = true;
    },
    beforePdfUpload(file) {
      console.log(file.type)
      return true
    },
    onPdfError(res, file) {
      console.log(res);
      console.log(file);
    },
    submitUpload() {
      this.$refs.upload.submit();
    },
  },
}
</script>

<style lang="css">
</style>
