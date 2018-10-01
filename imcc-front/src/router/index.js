import Vue from 'vue'
import Router from 'vue-router'
import upload from '@/components/upload'
import main from '@/components/main'

Vue.use(Router)

export default new Router({
  routes: [
    {
      path: "/upload",
      component: upload,
    },
    {
      path: "/main",
      component: main,
    },
    {
      path: "/",
      redirect: "/main"
    },
  ]
})
