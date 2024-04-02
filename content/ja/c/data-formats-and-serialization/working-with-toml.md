---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:45.945263-07:00
description: "TOML\uFF08Tom's Obvious, Minimal\u2026"
lastmod: '2024-03-13T22:44:42.826020-06:00'
model: gpt-4-0125-preview
summary: "TOML\uFF08Tom's Obvious, Minimal\u2026"
title: "TOML\u3092\u64CD\u4F5C\u3059\u308B"
weight: 39
---

## 何となぜ？

TOML（Tom's Obvious, Minimal Language）は、その明確なセマンティクスにより読みやすい設定ファイル形式です。プログラマーは、そのシンプルさと人間が読める特性のため、特定の文脈でXMLやJSONのような形式よりも優れた選択肢として、アプリケーションの設定ファイルにこれを使用します。

## どのように：

CでTOMLを扱うには、C標準ライブラリにはこの機能が含まれていないため、TOMLファイルを解析できるライブラリが最初に必要になります。「tomlc99」という、C99のための軽量TOMLパーサーが人気の選択肢です。以下は、シンプルなTOML設定ファイルを読み込むための簡単なガイドです：

まず、`tomlc99`が正しくインストールされ、プロジェクトに適切にリンクされていることを確認してください。

**サンプルTOMLファイル（`config.toml`）：**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**このファイルを解析するCコード：**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("ファイルを開けません");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "ファイルの解析エラー\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("データベースサーバー: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("ポート %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**出力：**
```
データベースサーバー: "192.168.1.1"
ポート 0: 8001
ポート 1: 8001
ポート 2: 8002
```

## 深く掘り下げる

TOMLは、GitHubの共同創設者であるTom Preston-Wernerによって、他の設定ファイル形式の制限に対する反応として作成されました。その目的は、複雑な解析ルールを必要とせずに、人間とコンピュータの両方が読み書きできるように、直接的で曖昧さのないものであることです。Cのエコシステム内では、Rustの`serde_toml`やPythonの`toml`のような高水準言語であるところのネイティブサポートを持つライブラリがあるように、TOMLは第一級の市民ではありません。しかし、C開発者は`tomlc99`のような外部ライブラリに依存する必要がありますが、これはCのミニマリズムとパフォーマンスへの重点を考えると典型的です。

TOMLはその明快さで賞賛されていますが、設定ファイル形式を選択する際には、プロジェクトのニーズを考慮することが重要です。より複雑な構造が必要な場合やウェブAPIとの相互運用が求められるシナリオでは、その増加した複雑さにもかかわらず、JSONやYAMLがより良い適合を提供するかもしれません。TOMLは、必ずしも最も高度なデータ構造が必要なわけではなく、可読性とシンプルさが最優先される設定において輝きます。
