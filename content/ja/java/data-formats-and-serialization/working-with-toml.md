---
aliases:
- /ja/java/working-with-toml/
date: 2024-01-26 04:23:06.157021-07:00
description: "TOML\u306FTom's Obvious, Minimal Language\u306E\u7565\u3067\u3059\u3002\
  \u3053\u308C\u306F\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u7528\u306E\u30C7\u30FC\
  \u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u5F62\u5F0F\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3053\u308C\u3092\u4F7F\u7528\
  \u3059\u308B\u7406\u7531\u306F\u3001\u8AAD\u307F\u66F8\u304D\u304C\u5BB9\u6613\u3067\
  \u3001\u30CF\u30C3\u30B7\u30E5\u30C6\u30FC\u30D6\u30EB\u306B\u3046\u307E\u304F\u30DE\
  \u30C3\u30D4\u30F3\u30B0\u3067\u304D\u308B\u304B\u3089\u3067\u3059\u3002"
lastmod: 2024-02-18 23:08:54.823469
model: gpt-4-0125-preview
summary: "TOML\u306FTom's Obvious, Minimal Language\u306E\u7565\u3067\u3059\u3002\u3053\
  \u308C\u306F\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u7528\u306E\u30C7\u30FC\u30BF\
  \u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u5F62\u5F0F\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3053\u308C\u3092\u4F7F\u7528\u3059\
  \u308B\u7406\u7531\u306F\u3001\u8AAD\u307F\u66F8\u304D\u304C\u5BB9\u6613\u3067\u3001\
  \u30CF\u30C3\u30B7\u30E5\u30C6\u30FC\u30D6\u30EB\u306B\u3046\u307E\u304F\u30DE\u30C3\
  \u30D4\u30F3\u30B0\u3067\u304D\u308B\u304B\u3089\u3067\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
---

{{< edit_this_page >}}

## 何を、なぜ？
TOMLはTom's Obvious, Minimal Languageの略です。これは、設定ファイル用のデータシリアライゼーション形式です。プログラマーがこれを使用する理由は、読み書きが容易で、ハッシュテーブルにうまくマッピングできるからです。

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
