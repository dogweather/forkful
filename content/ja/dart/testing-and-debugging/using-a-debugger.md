---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:40.553069-07:00
description: "\u65B9\u6CD5: **1. \u30D6\u30EC\u30FC\u30AF\u30DD\u30A4\u30F3\u30C8\u306E\
  \u8A2D\u5B9A:** \u30D6\u30EC\u30FC\u30AF\u30DD\u30A4\u30F3\u30C8\u3092\u8A2D\u5B9A\
  \u3059\u308B\u306B\u306F\u3001\u5B9F\u884C\u3092\u4E00\u6642\u505C\u6B62\u3055\u305B\
  \u305F\u3044\u30B3\u30FC\u30C9\u884C\u306E\u5DE6\u30DE\u30FC\u30B8\u30F3\u3092IDE\uFF08\
  \u4F8B\uFF1AVisual Studio Code \u3084 Android Studio\uFF09\u3067\u30AF\u30EA\u30C3\
  \u30AF\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T22:37:50.004384-06:00'
model: gpt-4-0125-preview
summary: "**1. \u30D6\u30EC\u30FC\u30AF\u30DD\u30A4\u30F3\u30C8\u306E\u8A2D\u5B9A\
  :** \u30D6\u30EC\u30FC\u30AF\u30DD\u30A4\u30F3\u30C8\u3092\u8A2D\u5B9A\u3059\u308B\
  \u306B\u306F\u3001\u5B9F\u884C\u3092\u4E00\u6642\u505C\u6B62\u3055\u305B\u305F\u3044\
  \u30B3\u30FC\u30C9\u884C\u306E\u5DE6\u30DE\u30FC\u30B8\u30F3\u3092IDE\uFF08\u4F8B\
  \uFF1AVisual Studio Code \u3084 Android Studio\uFF09\u3067\u30AF\u30EA\u30C3\u30AF\
  \u3057\u307E\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30AC\u306E\u4F7F\u7528"
weight: 35
---

## 方法:


### 基本的なデバッグ:
**1. ブレークポイントの設定:**

ブレークポイントを設定するには、実行を一時停止させたいコード行の左マージンをIDE（例：Visual Studio Code や Android Studio）でクリックします。

```dart
void main() {
  var message = 'Hello, Debugging';
  print(message); // ここにブレークポイントを設定
}
```

**2. デバッグの開始:**

IDEでデバッグアイコンをクリックするか、デバッグボタンを押してデバッグセッションを開始します。実行はブレークポイントで一時停止します。

**3. 変数の検査:**

実行が一時停止したら、変数にカーソルを合わせて現在の値を確認します。

**4. コードのステップスルー:**

IDEのステップオーバー、ステップイン、ステップアウトコマンドを使用して、コードを一行または一関数ずつナビゲートします。

### Observatoryを使用した高度なデバッグ:
Dartには、Dartアプリケーションのデバッグとプロファイリング用のツールであるObservatoryが含まれています。これは、Dart VM上で実行されているアプリケーションに特に便利です。

**Observatoryへのアクセス:**

`--observe`フラグを付けてDartアプリケーションを実行します。

```bash
dart --observe your_program.dart
```

このコマンドはコンソールにURLを出力します。そのURLをWebブラウザで開くと、Observatoryデバッガにアクセスできます。

### 人気のあるサードパーティライブラリの使用:
Flutterアプリケーションのデバッグには、`flutter_devtools`パッケージがDart VMとFlutterの両方に統合されたパフォーマンスとデバッグツールのスイートを提供します。

**インストール:**

まず、`pubspec.yaml`ファイルの`dev_dependencies`の下に`devtools`を追加します：

```yaml
dev_dependencies:
  devtools: any
```

**DevToolsの起動:**

ターミナルでこのコマンドを実行します：

```bash
flutter pub global run devtools
```

次に、Flutterアプリケーションをデバッグモードで起動します。DevToolsは、ウィジェットツリー分析のためのFlutterインスペクターや、ネットワーク活動の監視のためのネットワークプロファイラーなどの機能を提供します。

### サンプル出力:
ブレークポイントに到達すると、IDEは次のように変数の値とスタックトレースを表示する場合があります：

```
message: 'Hello, Debugging'
```

ダートでデバッグツールと技術を効果的に活用することで、開発者は問題をより迅速に特定し解決できるようになり、よりスムーズな開発プロセスとより堅牢なアプリケーションへとつながります。
