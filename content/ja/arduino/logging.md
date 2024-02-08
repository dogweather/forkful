---
title:                "ロギング"
date:                  2024-01-26T00:59:04.808896-07:00
model:                 gpt-4-1106-preview
simple_title:         "ロギング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/logging.md"
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
