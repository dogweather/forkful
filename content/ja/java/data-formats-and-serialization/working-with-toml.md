---
date: 2024-01-26 04:23:06.157021-07:00
description: "\u4F7F\u3044\u65B9\uFF1A TOML\u30D1\u30FC\u30B7\u30F3\u30B0\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u304C\u5FC5\u8981\u3067\u3059\u3002`toml4j`\u306E\u4F7F\u7528\
  \u3092\u304A\u52E7\u3081\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\
  \u306B\u6B21\u306E\u3088\u3046\u306B\u8FFD\u52A0\u3057\u3066\u304F\u3060\u3055\u3044\
  \uFF1A."
lastmod: '2024-04-05T21:53:42.870102-06:00'
model: gpt-4-0125-preview
summary: ''
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

## 使い方：
TOMLパーシングライブラリが必要です。`toml4j`の使用をお勧めします。プロジェクトに次のように追加してください：

```java
// これをbuild.gradleに追加
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

TOMLファイルのパース方法は次のとおりです：

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("サーバーIP: " + ip);
        System.out.println("サーバーポート: " + port);
    }
}
```

サンプル出力：

```
サーバーIP: 192.168.1.1
サーバーポート: 80
```

## 詳細解説
GitHubの共同創設者であるTom Preston-Wernerによって開発されたTOMLは、XMLよりも単純で、YAMLよりも具体的であることを目指しています。その最新バージョン1.0.0は、2021年にリリースされ、安定した機能セットを提供します。

JSONやYAMLのような代替品も人気があります。JSONはデータ交換に優れています。YAMLは複雑な設定に対してより人間が読みやすいです。TOMLの強みはその直感性とRustコミュニティでの使用です。

JavaでTOMLを使用する際の実装については、選択するパーサーが重要です。`toml4j`の他に、`jackson-dataformat-toml`を選ぶ人もいます。エラーハンドリングやパースのパフォーマンスなど、細かな違いがありますので、プロジェクトのニーズに基づいて選択してください。

## 参照
- TOML仕様：https://toml.io/en/
- `toml4j` GitHub：https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`：https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
