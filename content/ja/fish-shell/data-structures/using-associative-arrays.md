---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:01.339167-07:00
description: "\u65B9\u6CD5: Fish\u306FBash 4+\u306E\u3088\u3046\u306B\u30CD\u30A4\u30C6\
  \u30A3\u30D6\u306B\u9023\u60F3\u914D\u5217\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\
  \u3044\u307E\u305B\u3093\u304C\u3001\u30EA\u30B9\u30C8\u3068\u6587\u5B57\u5217\u64CD\
  \u4F5C\u306E\u7D44\u307F\u5408\u308F\u305B\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\
  \u3067\u3001\u540C\u69D8\u306E\u6A5F\u80FD\u3092\u5B9F\u73FE\u3067\u304D\u307E\u3059\
  \u3002\u3053\u3061\u3089\u304C\u305D\u308C\u3089\u306E\u6A21\u5023\u65B9\u6CD5\u3067\
  \u3059\uFF1A \u307E\u305A\u3001\u300C\u9023\u60F3\u914D\u5217\u300D\u8981\u7D20\u3092\
  \u5225\u3005\u306B\u8A2D\u5B9A\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.507886-06:00'
model: gpt-4-0125-preview
summary: "Fish\u306FBash 4+\u306E\u3088\u3046\u306B\u30CD\u30A4\u30C6\u30A3\u30D6\u306B\
  \u9023\u60F3\u914D\u5217\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u305B\
  \u3093\u304C\u3001\u30EA\u30B9\u30C8\u3068\u6587\u5B57\u5217\u64CD\u4F5C\u306E\u7D44\
  \u307F\u5408\u308F\u305B\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u3001\u540C\
  \u69D8\u306E\u6A5F\u80FD\u3092\u5B9F\u73FE\u3067\u304D\u307E\u3059\u3002\u3053\u3061\
  \u3089\u304C\u305D\u308C\u3089\u306E\u6A21\u5023\u65B9\u6CD5\u3067\u3059\uFF1A \u307E\
  \u305A\u3001\u300C\u9023\u60F3\u914D\u5217\u300D\u8981\u7D20\u3092\u5225\u3005\u306B\
  \u8A2D\u5B9A\u3057\u307E\u3059\uFF1A."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## 方法:
FishはBash 4+のようにネイティブに連想配列をサポートしていませんが、リストと文字列操作の組み合わせを使用することで、同様の機能を実現できます。こちらがそれらの模倣方法です：

まず、「連想配列」要素を別々に設定します：

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

要素にアクセスするには、直接参照します：

```Fish Shell
echo $food_color_apple
# 出力: red
```

それらを繰り返し処理する必要がある場合は、命名規則を考えたforループを使用します：

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# 出力:
# red
# yellow
```

Bashの`${!array[@]}`で全てのキーを取得する機能が恋しい場合は、キーを別のリストに保存できます：

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'is' $food_color_$key
end
# 出力:
# apple is red
# banana is yellow
```

## 詳細解説
他のスクリプト言語のような本物の連想配列は、まだFishのアプローチの一部ではありません。ここで示された回避策は、Fishの文字列操作とリスト機能を活用して、擬似的な連想配列構造を作成します。この方法は機能しますが、組み込みの連想配列サポートがあればもっとクリーンでエラーが少なくなるはずです。BashやZshなどの他のシェルは組み込みの連想配列機能を提供しており、より直接的で読みやすいコードになります。しかし、Fishの設計哲学はシンプルさとユーザーフレンドリーさを目指しており、そのような機能が犠牲になることもあります。この回避策はほとんどのニーズを満たしますが、Fish Shellの進化に注目してください。開発者はコミュニティのフィードバックに基づいて積極的に機能の改善と追加を行っています。
