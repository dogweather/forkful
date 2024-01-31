---
title:                "連想配列の使用"
date:                  2024-01-30T19:11:01.339167-07:00
model:                 gpt-4-0125-preview
simple_title:         "連想配列の使用"

category:             "Fish Shell"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となく理由

連想配列、またはハッシュマップは、データをキーと値のペアとして保存できるため、キーで情報を整理して取得しやすくなります。特に設定やさまざまな属性を扱う場合など、リストだけでは不十分なデータ処理をより構造化した方法で扱いたいときに便利です。

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
