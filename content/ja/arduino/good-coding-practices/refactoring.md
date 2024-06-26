---
date: 2024-01-26 01:17:19.040903-07:00
description: "\u65B9\u6CD5\uFF1A Arduino\u4E0A\u3067\u3042\u307E\u308A\u306B\u3082\
  \u591A\u304F\u306E\u3053\u3068\u3092\u3057\u3066\u3044\u308B\u95A2\u6570\u304C\u3042\
  \u308B\u3068\u3057\u307E\u3057\u3087\u3046\u3001\u4F8B\u3048\u3070\u3053\u3093\u306A\
  \u611F\u3058\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:38:42.015823-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Arduino\u4E0A\u3067\u3042\u307E\u308A\u306B\u3082\u591A\
  \u304F\u306E\u3053\u3068\u3092\u3057\u3066\u3044\u308B\u95A2\u6570\u304C\u3042\u308B\
  \u3068\u3057\u307E\u3057\u3087\u3046\u3001\u4F8B\u3048\u3070\u3053\u3093\u306A\u611F\
  \u3058\u3067\u3059\uFF1A."
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
weight: 19
---

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
