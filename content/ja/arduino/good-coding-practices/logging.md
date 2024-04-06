---
date: 2024-01-26 00:59:04.808896-07:00
description: "\u65B9\u6CD5: Arduino\u306B\u306F\u4ED6\u306E\u74B0\u5883\u306E\u3088\
  \u3046\u306A\u7D44\u307F\u8FBC\u307F\u306E\u30ED\u30B0\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u304C\u3042\u308A\u307E\u305B\u3093\u304C\u3001Serial\u30B3\u30F3\u30BD\u30FC\u30EB\
  \u3078\u306E\u57FA\u672C\u7684\u306A\u30ED\u30B0\u51FA\u529B\u3092\u6700\u5C0F\u9650\
  \u306E\u624B\u9593\u3067\u5B9F\u88C5\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\
  \u3059\u3002\u3053\u3053\u306B\u7C21\u5358\u306A\u4F8B\u3092\u793A\u3057\u307E\u3059\
  \u3002"
lastmod: '2024-04-05T21:53:43.319605-06:00'
model: gpt-4-1106-preview
summary: "Arduino\u306B\u306F\u4ED6\u306E\u74B0\u5883\u306E\u3088\u3046\u306A\u7D44\
  \u307F\u8FBC\u307F\u306E\u30ED\u30B0\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u3042\u308A\
  \u307E\u305B\u3093\u304C\u3001Serial\u30B3\u30F3\u30BD\u30FC\u30EB\u3078\u306E\u57FA\
  \u672C\u7684\u306A\u30ED\u30B0\u51FA\u529B\u3092\u6700\u5C0F\u9650\u306E\u624B\u9593\
  \u3067\u5B9F\u88C5\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\
  \u3053\u306B\u7C21\u5358\u306A\u4F8B\u3092\u793A\u3057\u307E\u3059\u3002"
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

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
