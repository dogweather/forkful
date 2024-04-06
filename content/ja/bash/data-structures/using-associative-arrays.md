---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:10.951532-07:00
description: "\u65B9\u6CD5\uFF1A \u9023\u60F3\u914D\u5217\u306FBash\u30D0\u30FC\u30B8\
  \u30E7\u30F34.0\u3067\u5C0E\u5165\u3055\u308C\u3001\u8A00\u8A9E\u306B\u6BD4\u8F03\
  \u7684\u6700\u8FD1\u8FFD\u52A0\u3055\u308C\u305F\u6A5F\u80FD\u3067\u3059\u3002\u305D\
  \u306E\u5C0E\u5165\u524D\u306B\u306F\u3001\u975E\u6574\u6570\u30A4\u30F3\u30C7\u30C3\
  \u30AF\u30B9\u914D\u5217\u3092\u6271\u3046\u3053\u3068\u306F\u3001`awk`\u3084`sed`\u306E\
  \u3088\u3046\u306A\u5916\u90E8\u30C4\u30FC\u30EB\u3084\u56DE\u907F\u7B56\u3092\u3057\
  \u3070\u3057\u3070\u5FC5\u8981\u3068\u3059\u308B\u9762\u5012\u306A\u3082\u306E\u3067\
  \u3057\u305F\u3002\u2026"
lastmod: '2024-04-05T22:50:56.261266-06:00'
model: gpt-4-0125-preview
summary: "\u5185\u90E8\u7684\u306B\u306F\u3001Bash\u306F\u30CF\u30C3\u30B7\u30E5\u30C6\
  \u30FC\u30D6\u30EB\u3092\u4F7F\u7528\u3057\u3066\u9023\u60F3\u914D\u5217\u3092\u5B9F\
  \u88C5\u3057\u3066\u3044\u307E\u3059\u3002\u3053\u306E\u5B9F\u88C5\u306F\u3001\u914D\
  \u5217\u306E\u30B5\u30A4\u30BA\u306B\u304B\u304B\u308F\u3089\u305A\u3001\u52B9\u7387\
  \u7684\u306A\u30AD\u30FC\u691C\u7D22\u3092\u53EF\u80FD\u306B\u3057\u3001\u30B9\u30AF\
  \u30EA\u30D7\u30C8\u5B9F\u884C\u306E\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u306B\
  \u3068\u3063\u3066\u91CD\u8981\u306A\u7279\u5FB4\u3092\u4FDD\u3061\u307E\u3059\u3002"
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

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
