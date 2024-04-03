---
date: 2024-01-26 01:17:19.040903-07:00
description: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3068\u306F\u3001\u5916\
  \u90E8\u306E\u52D5\u4F5C\u3084\u6A5F\u80FD\u3092\u5909\u66F4\u3059\u308B\u3053\u3068\
  \u306A\u304F\u3001\u30B3\u30FC\u30C9\u306E\u69CB\u9020\u3068\u53EF\u8AAD\u6027\u3092\
  \u6539\u5584\u3059\u308B\u305F\u3081\u306B\u884C\u3046\u30D7\u30ED\u30BB\u30B9\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u30B3\u30FC\u30C9\u3092\u3088\
  \u308A\u30AF\u30EA\u30FC\u30F3\u3067\u3001\u7406\u89E3\u3057\u3084\u3059\u304F\u3001\
  \u305D\u3057\u3066\u30E1\u30F3\u30C6\u30CA\u30F3\u30B9\u3057\u3084\u3059\u304F\u3059\
  \u308B\u305F\u3081\u306B\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u884C\
  \u3044\u307E\u3059\u3002\u3053\u308C\u306F\u9577\u671F\u7684\u306B\u306F\u3001\u30C7\
  \u30D0\u30C3\u30B0\u3084\u65B0\u6A5F\u80FD\u306E\u8FFD\u52A0\u3092\u305A\u3063\u3068\
  \u982D\u75DB\u306E\u7A2E\u304B\u3089\u89E3\u653E\u3057\u3066\u304F\u308C\u307E\u3059\
  \u3002"
lastmod: '2024-03-13T22:44:42.509186-06:00'
model: gpt-4-0125-preview
summary: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3068\u306F\u3001\u5916\
  \u90E8\u306E\u52D5\u4F5C\u3084\u6A5F\u80FD\u3092\u5909\u66F4\u3059\u308B\u3053\u3068\
  \u306A\u304F\u3001\u30B3\u30FC\u30C9\u306E\u69CB\u9020\u3068\u53EF\u8AAD\u6027\u3092\
  \u6539\u5584\u3059\u308B\u305F\u3081\u306B\u884C\u3046\u30D7\u30ED\u30BB\u30B9\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u30B3\u30FC\u30C9\u3092\u3088\
  \u308A\u30AF\u30EA\u30FC\u30F3\u3067\u3001\u7406\u89E3\u3057\u3084\u3059\u304F\u3001\
  \u305D\u3057\u3066\u30E1\u30F3\u30C6\u30CA\u30F3\u30B9\u3057\u3084\u3059\u304F\u3059\
  \u308B\u305F\u3081\u306B\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u884C\
  \u3044\u307E\u3059\u3002\u3053\u308C\u306F\u9577\u671F\u7684\u306B\u306F\u3001\u30C7\
  \u30D0\u30C3\u30B0\u3084\u65B0\u6A5F\u80FD\u306E\u8FFD\u52A0\u3092\u305A\u3063\u3068\
  \u982D\u75DB\u306E\u7A2E\u304B\u3089\u89E3\u653E\u3057\u3066\u304F\u308C\u307E\u3059\
  \u3002."
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
weight: 19
---

## 何となぜ？
リファクタリングとは、外部の動作や機能を変更することなく、コードの構造と可読性を改善するために行うプロセスです。プログラマは、コードをよりクリーンで、理解しやすく、そしてメンテナンスしやすくするためにリファクタリングを行います。これは長期的には、デバッグや新機能の追加をずっと頭痛の種から解放してくれます。

## 方法：

Arduino上であまりにも多くのことをしている関数があるとしましょう、例えばこんな感じです：

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // あまりにも多くのことをしている関数
  handleEverything();
}

void handleEverything() {
  // センサーデータを読む
  int sensorValue = analogRead(A0);
  // センサーデータを処理する
  sensorValue = map(sensorValue, 0, 1023, 0, 255);
  // センサーデータを出力する
  Serial.println(sensorValue);
  delay(500);
}
```

リファクタリングを行うと、`handleEverything()`をもっと小さく、焦点を絞った関数に分割することができます：

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = readSensorData();
  int processedValue = processSensorData(sensorValue);
  printData(processedValue);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorValue) {
  return map(sensorValue, 0, 1023, 0, 255);
}

void printData(int data) {
  Serial.println(data);
}
```

リファクタリング後、`loop()`関数はより読みやすくなり、各タスクが専用の関数によって処理されるため、コードの管理が容易になります。

## 深堀り
歴史的に、リファクタリングはアジャイルやテスト駆動開発（TDD）の方法論の台頭とともに人気を博しました。これらの方法論は、変化する要件に適応するためにコードの継続的な改善に依存しています。リファクタリングには、Arduinoの例で使用した「メソッドの抽出」技術のような、さまざまなツールと戦略があります。これは、あなたがクイックプロトタイプから安定したプロジェクトに移行するとき、コードの可読性とメンテナンスが重要になる場合に不可欠です。

リファクタリングを行う際は、変更がバグを導入していないことを保証するために、良いセットのテストがあることが重要です。Arduinoの世界では、ハードウェア依存のため自動テストは常に簡単なわけではありませんが、純粋な論理部分に対してユニットテストを使用したり、シミュレータを活用したりすることができます。

手動リファクタリングの代わりに専用のリファクタリングツールを使用することもできます。これらはコードの臭いを自動的に識別し、変更を提案します。しかし、これらのツールはしばしばマイクロコントローラのコードのニュアンスに欠けているかもしれず、Arduino開発環境では利用できないことがあります。

最終的に、リファクタリングはコードの内部構造を改善することと、欠陥を導入するリスクをバランスさせる芸術です。特にマイクロコントローラのリソースが制約されているため、メモリ使用量やプロセッサ時間などの実装の詳細について考えることが求められます。

## 参照
リファクタリングについてさらに深く掘り下げるには、マーティン・ファウラーの基本書「Refactoring: Improving the Design of Existing Code」を読むことをお勧めします。Arduino固有の実践についてより詳しく知るには、Arduino開発フォーラムやコミュニティをチェックしてください：

- [Arduino Forum - Programming Questions](https://forum.arduino.cc/index.php?board=4.0)
- [リファクタリング グル](https://refactoring.guru/refactoring)

覚えておいてください、目標は未来のあなたと他の人が感謝するクリーンでわかりやすいコードです。ハッキングを続け、それを整理しましょう！
