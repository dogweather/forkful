---
title:                "TOMLを扱う方法"
aliases:
- /ja/java/working-with-toml.md
date:                  2024-01-26T04:23:06.157021-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/working-with-toml.md"
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
