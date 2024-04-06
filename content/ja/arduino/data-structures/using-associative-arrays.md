---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:20.984580-07:00
description: "\u65B9\u6CD5\uFF1A \u6B74\u53F2\u7684\u306B\u3001C\u304A\u3088\u3073\
  C++(Arduino\u306E\u69CB\u6587\u304C\u6D3E\u751F\u3057\u3066\u3044\u308B\u8A00\u8A9E\
  )\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\u9023\u60F3\u914D\u5217\u304C\u306A\u304F\
  \u3001\u4E0A\u306B\u793A\u3057\u305F\u3088\u3046\u306A\u56DE\u907F\u7B56\u304C\u5FC5\
  \u8981\u3067\u3057\u305F\u3002\u3053\u306E\u30A2\u30D7\u30ED\u30FC\u30C1\u306F\u6BD4\
  \u8F03\u7684\u30B7\u30F3\u30D7\u30EB\u3067\u3059\u304C\u3001\u30C7\u30FC\u30BF\u30B5\
  \u30A4\u30BA\u304C\u5897\u52A0\u3059\u308B\u306B\u3064\u308C\u3066O(n)\u306E\u691C\
  \u7D22\u6642\u9593\u306E\u305F\u3081\u306B\u30B9\u30B1\u30FC\u30E9\u30D3\u30EA\u30C6\
  \u30A3\u304C\u4F4E\u4E0B\u3057\u307E\u3059\u3002\u2026"
lastmod: '2024-04-05T22:38:41.993835-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A \u53B3\u5BC6\u306B\u8A00\u3048\u3070\u3001Arduino\u306B\
  \u306F\u9AD8\u30EC\u30D9\u30EB\u8A00\u8A9E\u306B\u898B\u3089\u308C\u308B\u3088\u3046\
  \u306A\u7D44\u307F\u8FBC\u307F\u306E\u9023\u60F3\u914D\u5217\u306E\u30B5\u30DD\u30FC\
  \u30C8\u306F\u3042\u308A\u307E\u305B\u3093\u3002\u3057\u304B\u3057\u3001\u6050\u308C\
  \u308B\u3053\u3068\u306F\u3042\u308A\u307E\u305B\u3093\u3002\u69CB\u9020\u4F53\u3084\
  \u914D\u5217\u3092\u4F7F\u7528\u3057\u3066\u3053\u306E\u6A5F\u80FD\u3092\u6A21\u5023\
  \u3059\u308B\u30AF\u30E9\u30D5\u30C8\u7684\u306A\u65B9\u6CD5\u304C\u3042\u308A\u307E\
  \u3059\u3002\u3053\u3053\u3067\u306F\u3001\u7570\u306A\u308B\u90FD\u5E02\u306E\u6E29\
  \u5EA6\u3092\u4FDD\u5B58\u304A\u3088\u3073\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u305F\
  \u3081\u306E\u57FA\u672C\u7684\u306A\u300C\u9023\u60F3\u914D\u5217\u300D\u3092\u4F5C\
  \u6210\u3059\u308B\u305F\u3081\u306E\u7C21\u5358\u306A\u4F8B\u3092\u7D39\u4ECB\u3057\
  \u307E\u3059\u3002 \u307E\u305A\u3001\u90FD\u5E02\uFF08\u30AD\u30FC\uFF09\u3068\u305D\
  \u306E\u6E29\u5EA6\uFF08\u5024\uFF09\u3092\u4FDD\u6301\u3059\u308B\u305F\u3081\u306E\
  \u69CB\u9020\u4F53\u3092\u5B9A\u7FA9\u3057\u307E\u3059\uFF1A."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## 方法：
厳密に言えば、Arduinoには高レベル言語に見られるような組み込みの連想配列のサポートはありません。しかし、恐れることはありません。構造体や配列を使用してこの機能を模倣するクラフト的な方法があります。ここでは、異なる都市の温度を保存およびアクセスするための基本的な「連想配列」を作成するための簡単な例を紹介します。

まず、都市（キー）とその温度（値）を保持するための構造体を定義します：

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

次に、`CityTemperature`オブジェクトの配列を初期化します：

```cpp
CityTemperature temperatures[] = {
  {"New York", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

特定の都市の温度にアクセスして表示する方法は次のとおりです：

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("The temperature in Los Angeles is: ");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // 今はここに何もない。
}
```

このコードを実行すると、次の出力が得られます：

```
The temperature in Los Angeles is: 22.0
```

## 詳細な調査
歴史的に、CおよびC++(Arduinoの構文が派生している言語)には組み込みの連想配列がなく、上に示したような回避策が必要でした。このアプローチは比較的シンプルですが、データサイズが増加するにつれてO(n)の検索時間のためにスケーラビリティが低下します。

Pythonのような言語は辞書を提供し、JavaScriptにはこの目的のためのオブジェクトがあり、いずれもキー-値ペアを管理するにははるかに効率的です。Arduinoでパフォーマンスと効率が重要になったとき、開発者はライブラリを介して実装されるハッシュテーブルなどのより専門的なデータ構造を選択するかもしれません。

Arduinoがネイティブに連想配列をサポートしていなくても、`HashMap`のようなライブラリをプロジェクトに追加して同様の機能をDIYアプローチよりも優れたパフォーマンスで提供できるように開発されたコミュニティがあります。これらのライブラリは、特に複雑なプロジェクトのために、連想配列を管理するよりエレガントで効率的な手段を通常提供します。
