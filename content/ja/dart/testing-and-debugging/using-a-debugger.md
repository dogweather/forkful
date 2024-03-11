---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:40.553069-07:00
description: "Dart\u3067\u30C7\u30D0\u30C3\u30AC\u3092\u4F7F\u7528\u3059\u308B\u3053\
  \u3068\u306B\u3088\u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30D6\u30EC\
  \u30FC\u30AF\u30DD\u30A4\u30F3\u30C8\u306E\u8A2D\u5B9A\u3001\u5B9F\u884C\u306E\u30B9\
  \u30C6\u30C3\u30D7\u30B9\u30EB\u30FC\u3001\u5909\u6570\u306E\u691C\u67FB\u3092\u884C\
  \u3044\u306A\u304C\u3089\u3001\u30E1\u30BD\u30C3\u30C9\u7684\u306B\u30B3\u30FC\u30C9\
  \u3092\u8ABF\u67FB\u3067\u304D\u307E\u3059\u3002\u3053\u306E\u30D7\u30ED\u30BB\u30B9\
  \u306F\u3001\u30D0\u30B0\u3092\u52B9\u7387\u7684\u306B\u7279\u5B9A\u304A\u3088\u3073\
  \u4FEE\u6B63\u3059\u308B\u305F\u3081\u306B\u4E0D\u53EF\u6B20\u3067\u3042\u308A\u3001\
  \u3057\u305F\u304C\u3063\u3066\u3001\u958B\u767A\u30E9\u30A4\u30D5\u30B5\u30A4\u30AF\
  \u30EB\u3067\u6B20\u304B\u305B\u306A\u3044\u30C4\u30FC\u30EB\u3068\u306A\u3063\u3066\
  \u3044\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.310207-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u30C7\u30D0\u30C3\u30AC\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\
  \u306B\u3088\u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30D6\u30EC\u30FC\
  \u30AF\u30DD\u30A4\u30F3\u30C8\u306E\u8A2D\u5B9A\u3001\u5B9F\u884C\u306E\u30B9\u30C6\
  \u30C3\u30D7\u30B9\u30EB\u30FC\u3001\u5909\u6570\u306E\u691C\u67FB\u3092\u884C\u3044\
  \u306A\u304C\u3089\u3001\u30E1\u30BD\u30C3\u30C9\u7684\u306B\u30B3\u30FC\u30C9\u3092\
  \u8ABF\u67FB\u3067\u304D\u307E\u3059\u3002\u3053\u306E\u30D7\u30ED\u30BB\u30B9\u306F\
  \u3001\u30D0\u30B0\u3092\u52B9\u7387\u7684\u306B\u7279\u5B9A\u304A\u3088\u3073\u4FEE\
  \u6B63\u3059\u308B\u305F\u3081\u306B\u4E0D\u53EF\u6B20\u3067\u3042\u308A\u3001\u3057\
  \u305F\u304C\u3063\u3066\u3001\u958B\u767A\u30E9\u30A4\u30D5\u30B5\u30A4\u30AF\u30EB\
  \u3067\u6B20\u304B\u305B\u306A\u3044\u30C4\u30FC\u30EB\u3068\u306A\u3063\u3066\u3044\
  \u307E\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30AC\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？

Dartでデバッガを使用することにより、プログラマーはブレークポイントの設定、実行のステップスルー、変数の検査を行いながら、メソッド的にコードを調査できます。このプロセスは、バグを効率的に特定および修正するために不可欠であり、したがって、開発ライフサイクルで欠かせないツールとなっています。

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
