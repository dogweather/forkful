---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:06.152013-07:00
description: "C\u8A00\u8A9E\u3067\u306E\u30ED\u30B0\u53D6\u308A\u306F\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u306E\u5B9F\u884C\u4E2D\u306E\u6D41\u308C\u3084\u6CE8\u76EE\u3059\
  \u3079\u304D\u30A4\u30D9\u30F3\u30C8\u3092\u8A18\u9332\u3057\u3001\u305D\u306E\u6319\
  \u52D5\u3084\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u306E\u5177\u4F53\u7684\u306A\
  \u30EC\u30D3\u30E5\u30FC\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30D0\u30C3\u30B0\u76EE\u7684\u3001\u30BD\u30D5\
  \u30C8\u30A6\u30A7\u30A2\u306E\u5065\u5EB7\u3092\u76E3\u8996\u3057\u3001\u30B7\u30B9\
  \u30C6\u30E0\u306E\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u3092\u78BA\u4FDD\u3059\u308B\
  \u305F\u3081\u306B\u30ED\u30B0\u53D6\u308A\u3092\u5229\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.806422-06:00'
model: gpt-4-0125-preview
summary: "C\u8A00\u8A9E\u3067\u306E\u30ED\u30B0\u53D6\u308A\u306F\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u306E\u5B9F\u884C\u4E2D\u306E\u6D41\u308C\u3084\u6CE8\u76EE\u3059\
  \u3079\u304D\u30A4\u30D9\u30F3\u30C8\u3092\u8A18\u9332\u3057\u3001\u305D\u306E\u6319\
  \u52D5\u3084\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u306E\u5177\u4F53\u7684\u306A\
  \u30EC\u30D3\u30E5\u30FC\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30D0\u30C3\u30B0\u76EE\u7684\u3001\u30BD\u30D5\
  \u30C8\u30A6\u30A7\u30A2\u306E\u5065\u5EB7\u3092\u76E3\u8996\u3057\u3001\u30B7\u30B9\
  \u30C6\u30E0\u306E\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u3092\u78BA\u4FDD\u3059\u308B\
  \u305F\u3081\u306B\u30ED\u30B0\u53D6\u308A\u3092\u5229\u7528\u3057\u307E\u3059\u3002\
  ."
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

## 方法：
C言語において、ログ取りは基本的なファイル操作またはより洗練されたライブラリを使用して達成することができます。簡単に始めるために、標準のI/Oライブラリから始めます。以下のスニペットは、基本的なログ実装を示しています。

シンプルなメッセージをログに記録するには：

```c
#include <stdio.h>

int main() {
    FILE *logFile;
    logFile = fopen("application.log", "a"); // ログファイルを追記モードで開く
    
    if (logFile == NULL) {
        perror("ログファイルのオープンに失敗しました。");
        return -1;
    }
    
    fprintf(logFile, "アプリケーションの起動。\n");
    
    // ここにあなたのアプリケーションロジック
    
    fprintf(logFile, "アプリケーションが正常に終了しました。\n");
    fclose(logFile);
    
    return 0;
}
```

`application.log` に出力される内容：

```
アプリケーションの起動。
アプリケーションが正常に終了しました。
```

タイムスタンプとログレベルを含む詳細なログを含むには：

```c
#include <stdio.h>
#include <time.h>

void logMessage(FILE *logFile, const char* level, const char* message) {
    time_t now;
    time(&now);
    char* datetime = ctime(&now);
    datetime[strlen(datetime)-1] = '\0'; // 改行文字を削除
    fprintf(logFile, "[%s] %s - %s\n", datetime, level, message);
}

int main() {
    FILE *logFile;
    logFile = fopen("detailed.log", "a");
    
    if (logFile == NULL) {
        perror("ログファイルのオープンに失敗しました。");
        return -1;
    }
    
    logMessage(logFile, "INFO", "アプリケーション起動中");
    // ここにあなたのアプリケーションロジック
    logMessage(logFile, "ERROR", "一例としてのエラー");
    
    fclose(logFile);
    
    return 0;
}
```

`detailed.log` に出力される内容：

```
[Thu Mar 10 14:32:01 2023] INFO - アプリケーション起動中
[Thu Mar 10 14:32:02 2023] ERROR - 一例としてのエラー
```

## 詳細分析
示されたように、C言語でのログ取りは単純なファイル操作に依存しており、Pythonの `logging` モジュールやJavaの `Log4j` など、他の言語のログ取り機能と比べると、強力さや柔軟性には欠けます。C言語でより高度なログ取り機能を利用するために、開発者はUnix系システム上の `syslog` のような、システム全体のログ管理を提供するライブラリや、`log4c` のようなサードパーティ製のライブラリに頻繁に頼ります。

歴史的に見て、ログ取りはプログラミングの不可欠な部分であり、プログラムの流れやエラーを追跡し理解することは、主に物理的な印刷出力を通じて行われていました。システムが進化するにつれて、ログ取りはより洗練され、現在では重大度の異なるレベル、ログローテーション、非同期ログ取りをサポートしています。

Cの標準ライブラリはログ取りを実装するための基本的なツールを提供していますが、その限界はしばしばカスタムのログフレームワークの作成や、より機能豊富で柔軟なログ取りソリューションのための外部ライブラリの採用につながります。これらの限界にもかかわらず、特に外部依存関係を最小限に抑える環境において、C言語での基本的なログ取りを理解し実装することは、デバッグやソフトウェアのメンテナンスに不可欠です。
