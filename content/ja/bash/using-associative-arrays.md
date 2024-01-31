---
title:                "連想配列の使用"
date:                  2024-01-30T19:10:10.951532-07:00
model:                 gpt-4-0125-preview
simple_title:         "連想配列の使用"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

連想配列は、整数だけでなく文字列をインデックスとして使用できるようにした、超充電された配列のようなものです。プログラマーは、より複雑なデータ構造のためにそれらを使用し、順番のリストにきちんと収まらないデータを扱うことを容易にします。

## 方法：

まず、Bashで連想配列を宣言します：

```Bash
declare -A my_array
```

次に、文字列をキーとして使用して値を入力し始めることができます：

```Bash
my_array["name"]="Linux Journal"
my_array["topic"]="Programming"
```

要素にアクセスするには、そのキーを使用します：

```Bash
echo ${my_array["name"]}  # 出力：Linux Journal
```

キーと値を反復処理することも簡単です：

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

サンプル出力は次のようになります：

```
name: Linux Journal
topic: Programming
```

要素を追加または変更するには、初期の入力のようにキーに値を割り当てるだけです：

```Bash
my_array["readers"]="You"
```

要素を削除するには`unset`を使用します：

```Bash
unset my_array["topic"]
```

## 深堀り

連想配列はBashバージョン4.0で導入され、言語に比較的最近追加された機能です。その導入前には、非整数インデックス配列を扱うことは、`awk`や`sed`のような外部ツールや回避策をしばしば必要とする面倒なものでした。

内部的には、Bashはハッシュテーブルを使用して連想配列を実装しています。この実装は、配列のサイズにかかわらず、効率的なキー検索を可能にし、スクリプト実行のパフォーマンスにとって重要な特徴を保ちます。

連想配列はBashに多くの力と柔軟性をもたらしますが、PythonやJavaScriptのような高レベル言語の配列に比べて扱いにくいという限定もあります。複雑なデータ操作タスクの場合、ジョブにより適した外部ツールや言語を検討する価値があるかもしれません。

しかし、多くの典型的なスクリプトタスクにおいて、連想配列はBashプログラマーのツールキットにおいて貴重なツールを提供し、数値インデックスの代わりに意味のある文字列キーの使用を可能にすることで、より読みやすく保守しやすいスクリプトを実現します。
