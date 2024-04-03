---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:06.152013-07:00
description: "\u65B9\u6CD5\uFF1A C\u8A00\u8A9E\u306B\u304A\u3044\u3066\u3001\u30ED\
  \u30B0\u53D6\u308A\u306F\u57FA\u672C\u7684\u306A\u30D5\u30A1\u30A4\u30EB\u64CD\u4F5C\
  \u307E\u305F\u306F\u3088\u308A\u6D17\u7DF4\u3055\u308C\u305F\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u4F7F\u7528\u3057\u3066\u9054\u6210\u3059\u308B\u3053\u3068\u304C\u3067\
  \u304D\u307E\u3059\u3002\u7C21\u5358\u306B\u59CB\u3081\u308B\u305F\u3081\u306B\u3001\
  \u6A19\u6E96\u306EI/O\u30E9\u30A4\u30D6\u30E9\u30EA\u304B\u3089\u59CB\u3081\u307E\
  \u3059\u3002\u4EE5\u4E0B\u306E\u30B9\u30CB\u30DA\u30C3\u30C8\u306F\u3001\u57FA\u672C\
  \u7684\u306A\u30ED\u30B0\u5B9F\u88C5\u3092\u793A\u3057\u3066\u3044\u307E\u3059\u3002\
  \ \u30B7\u30F3\u30D7\u30EB\u306A\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u30ED\u30B0\
  \u306B\u8A18\u9332\u3059\u308B\u306B\u306F\uFF1A."
lastmod: '2024-03-13T22:44:42.806422-06:00'
model: gpt-4-0125-preview
summary: "C\u8A00\u8A9E\u306B\u304A\u3044\u3066\u3001\u30ED\u30B0\u53D6\u308A\u306F\
  \u57FA\u672C\u7684\u306A\u30D5\u30A1\u30A4\u30EB\u64CD\u4F5C\u307E\u305F\u306F\u3088\
  \u308A\u6D17\u7DF4\u3055\u308C\u305F\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\
  \u3057\u3066\u9054\u6210\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\
  \u7C21\u5358\u306B\u59CB\u3081\u308B\u305F\u3081\u306B\u3001\u6A19\u6E96\u306EI/O\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u304B\u3089\u59CB\u3081\u307E\u3059\u3002\u4EE5\u4E0B\u306E\
  \u30B9\u30CB\u30DA\u30C3\u30C8\u306F\u3001\u57FA\u672C\u7684\u306A\u30ED\u30B0\u5B9F\
  \u88C5\u3092\u793A\u3057\u3066\u3044\u307E\u3059."
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
