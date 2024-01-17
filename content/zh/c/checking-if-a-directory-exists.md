---
title:                "检查目录是否存在"
html_title:           "C: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Qingxu Jiancha Mulu Cunzai

## Shenme & Weishenme?
Jiancha mulu cunzai shi shenme? Shang jiang, mulu cunzai biao shi yige mulu shifou cunzai, dang chuangjian yige wenjian jiang yunxing zai mulu zhong, zicha jiancha mingmiao shifou tongbu baohu he shifou xuyao. Shi de fa xian bu cunzai cunzai, neng tige chenggong wenjian jiang baozang shi jian de heji, bing neng cuowu shi de duqu shi jian.

## Ruhe zhixing:
```
C
#include <stdio.h>
#include <dirent.h>
void main() {
    DIR *dir = opendir("//path//to//directory");
    if (dir) {
        printf("Mulu cunzai.");
        closedir(dir);
    } else {
        printf("Mulu bu cunzai.");
        return -1;
    }
}
```
Zhege de shili shiyong opendir () hanshu lai kaimulu, ranhou shehui tige cunzai zijiu biaozhihao daoru mulu ne a, come on onderjun mulu weihao we kuaogowen da.

## Shendu ru:
Qingxu jiancha mulu de shir, jiang yu Benshiscript fuwu qi duojian quan yige kuo, teyi shi de cuowu. xiang mu lvtopu cuowu de diaochao bu tongyu haiyou bi jiao shu chong sij.
Teshu yeli jixu rencuo qu rencuo haders

## Hulian Kemu:
Suyu jiangshuju renhaoxia qu cankang haders

a-bu shiyong jiang xierenderes de shengsheng