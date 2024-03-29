---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:10.951532-07:00
description: "\u9023\u60F3\u914D\u5217\u306F\u3001\u6574\u6570\u3060\u3051\u3067\u306A\
  \u304F\u6587\u5B57\u5217\u3092\u30A4\u30F3\u30C7\u30C3\u30AF\u30B9\u3068\u3057\u3066\
  \u4F7F\u7528\u3067\u304D\u308B\u3088\u3046\u306B\u3057\u305F\u3001\u8D85\u5145\u96FB\
  \u3055\u308C\u305F\u914D\u5217\u306E\u3088\u3046\u306A\u3082\u306E\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u3088\u308A\u8907\u96D1\u306A\u30C7\
  \u30FC\u30BF\u69CB\u9020\u306E\u305F\u3081\u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\
  \u3057\u3001\u9806\u756A\u306E\u30EA\u30B9\u30C8\u306B\u304D\u3061\u3093\u3068\u53CE\
  \u307E\u3089\u306A\u3044\u30C7\u30FC\u30BF\u3092\u6271\u3046\u3053\u3068\u3092\u5BB9\
  \u6613\u306B\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.358655-06:00'
model: gpt-4-0125-preview
summary: "\u9023\u60F3\u914D\u5217\u306F\u3001\u6574\u6570\u3060\u3051\u3067\u306A\
  \u304F\u6587\u5B57\u5217\u3092\u30A4\u30F3\u30C7\u30C3\u30AF\u30B9\u3068\u3057\u3066\
  \u4F7F\u7528\u3067\u304D\u308B\u3088\u3046\u306B\u3057\u305F\u3001\u8D85\u5145\u96FB\
  \u3055\u308C\u305F\u914D\u5217\u306E\u3088\u3046\u306A\u3082\u306E\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u3088\u308A\u8907\u96D1\u306A\u30C7\
  \u30FC\u30BF\u69CB\u9020\u306E\u305F\u3081\u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\
  \u3057\u3001\u9806\u756A\u306E\u30EA\u30B9\u30C8\u306B\u304D\u3061\u3093\u3068\u53CE\
  \u307E\u3089\u306A\u3044\u30C7\u30FC\u30BF\u3092\u6271\u3046\u3053\u3068\u3092\u5BB9\
  \u6613\u306B\u3057\u307E\u3059\u3002"
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
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
