import Vue from 'vue'
import Router from 'vue-router'
import VueResource from 'vue-resource'
import upload from '@/components/upload'
import papers from '@/components/papers'
import main from '@/components/main'

Vue.use(Router)
Vue.use(VueResource)

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
      path: "/papers",
      component: papers,
    },
    {
      path: "/",
      redirect: "/main"
    },
  ]
})
