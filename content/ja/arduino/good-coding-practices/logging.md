---
date: 2024-01-26 00:59:04.808896-07:00
description: "\u300C\u30ED\u30B0\u300D\u306F\u3001\u3042\u308B\u6642\u9593\u5185\u306B\
  \u30B7\u30B9\u30C6\u30E0\u3067\u767A\u751F\u3059\u308B\u30A4\u30D9\u30F3\u30C8\u3001\
  \u30C8\u30E9\u30F3\u30B6\u30AF\u30B7\u30E7\u30F3\u3001\u307E\u305F\u306F\u6D3B\u52D5\
  \u306E\u8A18\u9332\u3092\u4FDD\u6301\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30D0\u30C3\u30B0\
  \u3001\u30B7\u30B9\u30C6\u30E0\u306E\u5065\u5168\u6027\u3092\u30E2\u30CB\u30BF\u30EA\
  \u30F3\u30B0\u3001\u7D71\u8A08\u306E\u53CE\u96C6\u3001\u3042\u308B\u3044\u306F\u4F7F\
  \u7528\u72B6\u6CC1\u306E\u76E3\u67FB\u306A\u3069\u306E\u76EE\u7684\u3067\u3053\u308C\
  \u3092\u4F7F\u7528\u3057\u3001\u3055\u307E\u3056\u307E\u306A\u6761\u4EF6\u4E0B\u3067\
  \u306E\u30B3\u30FC\u30C9\u306E\u6319\u52D5\u3092\u7DAD\u6301\u3057\u7406\u89E3\u3059\
  \u308B\u305F\u3081\u306B\u4E0D\u53EF\u6B20\u306A\u5B9F\u8DF5\u3067\u3059\u3002"
lastmod: 2024-02-19 22:05:01.621544
model: gpt-4-1106-preview
summary: "\u300C\u30ED\u30B0\u300D\u306F\u3001\u3042\u308B\u6642\u9593\u5185\u306B\
  \u30B7\u30B9\u30C6\u30E0\u3067\u767A\u751F\u3059\u308B\u30A4\u30D9\u30F3\u30C8\u3001\
  \u30C8\u30E9\u30F3\u30B6\u30AF\u30B7\u30E7\u30F3\u3001\u307E\u305F\u306F\u6D3B\u52D5\
  \u306E\u8A18\u9332\u3092\u4FDD\u6301\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30D0\u30C3\u30B0\
  \u3001\u30B7\u30B9\u30C6\u30E0\u306E\u5065\u5168\u6027\u3092\u30E2\u30CB\u30BF\u30EA\
  \u30F3\u30B0\u3001\u7D71\u8A08\u306E\u53CE\u96C6\u3001\u3042\u308B\u3044\u306F\u4F7F\
  \u7528\u72B6\u6CC1\u306E\u76E3\u67FB\u306A\u3069\u306E\u76EE\u7684\u3067\u3053\u308C\
  \u3092\u4F7F\u7528\u3057\u3001\u3055\u307E\u3056\u307E\u306A\u6761\u4EF6\u4E0B\u3067\
  \u306E\u30B3\u30FC\u30C9\u306E\u6319\u52D5\u3092\u7DAD\u6301\u3057\u7406\u89E3\u3059\
  \u308B\u305F\u3081\u306B\u4E0D\u53EF\u6B20\u306A\u5B9F\u8DF5\u3067\u3059\u3002"
title: "\u30ED\u30AE\u30F3\u30B0"
---

{{< edit_this_page >}}

## 何となぜ？
「ログ」は、ある時間内にシステムで発生するイベント、トランザクション、または活動の記録を保持することを指します。プログラマーは、デバッグ、システムの健全性をモニタリング、統計の収集、あるいは使用状況の監査などの目的でこれを使用し、さまざまな条件下でのコードの挙動を維持し理解するために不可欠な実践です。

## 方法:
Arduinoには他の環境のような組み込みのログライブラリがありませんが、Serialコンソールへの基本的なログ出力を最小限の手間で実装することができます。ここに簡単な例を示します。

```arduino
void setup() {
  // 与えられたボーレートでシリアル通信を開始
  Serial.begin(9600);

  // シリアルポートが接続するのを待つ - 一部のボードで必要
  while (!Serial) {
    ; // シリアルポートが接続されるのを待つ。USB直結の場合に必要
  }

  // セットアッププロセスが完了したことを伝える情報メッセージをログに記録
  Serial.println("Setup complete!");
}

void loop() {
  // 動作時間を毎秒プリントするシンプルなロガー
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Uptime (ms): ");
    Serial.println(currentMillis);

    // ここにはエラーログ、警告、その他の情報も追加できます。
  }
  
  // ここにプログラムの残りのロジックを書きます...
}
```

サンプルシリアル出力:
```
Setup complete!
Uptime (ms): 1000
Uptime (ms): 2000
Uptime (ms): 3000
...
```

## 深堀り:
歴史的には、マイクロコントローラー上でのログは、フルブローンのオペレーティングシステム上でのログほど単純な作業ではありませんでした。限られたリソースにより、バイトごとに重要が問われ、開発者はシステムを封鎖しないよう注意を払う必要がありました。より強力なボードが出現し、Arduinoプラットフォームがプロセスを簡素化するにつれて、ログはより容易になりました。

上記のコードでは、Serialインターフェイスを介したログのデモンストレーションを示しているものの、他の方法としては、SDカードへの書き込み、データをネットワーク経由でリモートサーバーに送信する、あるいは小型LCDへの出力などがあります。

ログシステムを実装する際には、ローテーション、重大度レベル（情報、デバッグ、警告、エラー）、および性能への影響などの考慮事項が発生します。Arduinoでは、複雑なデータ構造をログに記録する際にメモリの制約を念頭に置く必要があるかもしれません。リモートログにおいては、転送されるログのセキュリティも懸念事項です。

Syslogのような広く採用されているログ標準といった、Arduinoの世界の外部に存在するより洗練されたソリューションもありますが、さまざまな複雑さとリソース要件を持つ類似の機能を提供するサードパーティライブラリを統合することができます。

## 参照:
- [Arduino `Serial` リファレンス](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [ArduinoでのSDカードログ記録](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [SparkFunのデータログシールド](https://www.sparkfun.com/products/13712)
- [TinyWeb: Arduinoを使ったリモートログの実用例](https://www.arduino.cc/en/Tutorial/WebClientRepeating)
