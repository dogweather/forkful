---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:55.773015-07:00
description: "\u65B9\u6CD5\uFF1A 1. **Dart\u306E\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\
  **\uFF1A \u30B7\u30B9\u30C6\u30E0\u306BDart\u304C\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\
  \u3055\u308C\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u307E\u3059\u3002\
  \u305D\u3046\u3067\u306A\u3044\u5834\u5408\u306F\u3001[https://dart.dev/get-dart](https://dart.dev/get-dart)\u2026"
lastmod: '2024-04-05T22:37:49.999076-06:00'
model: gpt-4-0125-preview
summary: "//dart.dev/get-dart](https://dart.dev/get-dart) \u304B\u3089\u30C0\u30A6\
  \u30F3\u30ED\u30FC\u30C9\u3067\u304D\u307E\u3059\u3002\u30A4\u30F3\u30B9\u30C8\u30FC\
  \u30EB\u3092\u4EE5\u4E0B\u306E\u30B3\u30DE\u30F3\u30C9\u3067\u78BA\u8A8D\u3057\u3066\
  \u304F\u3060\u3055\u3044\uFF1A."
title: "\u65B0\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306E\u958B\u59CB"
weight: 1
---

## 方法：
1. **Dartのインストール**：
   システムにDartがインストールされていることを確認します。そうでない場合は、[https://dart.dev/get-dart](https://dart.dev/get-dart) からダウンロードできます。インストールを以下のコマンドで確認してください：

   ```shell
   dart --version
   ```

2. **新しいDartプロジェクトを作成する**：
   Dart CLIを使って新しいプロジェクトを生成します：

   ```shell
   dart create hello_dart
   ```

   このコマンドは、選択に応じて、シンプルなサンプルウェブかコンソールアプリケーションを含む新しいディレクトリ `hello_dart` を作成します。

3. **プロジェクトの構造を調べる**：
   
   プロジェクトディレクトリに移動します：

   ```shell
   cd hello_dart
   ```

   典型的なDartプロジェクトには、以下の主要なファイルとディレクトリが含まれます：

   - `pubspec.yaml`：プロジェクトの依存関係とSDKの制約を含む設定ファイル。
   - `lib/`：ほとんどのDartコードが存在するディレクトリ。
   - `test/`：プロジェクトテストのためのディレクトリ。

4. **依存関係を追加する**：
   `pubspec.yaml` を編集して、依存関係を追加します。ウェブプロジェクトの場合は、HTTPリクエストを行うための人気パッケージ `http` を追加することを検討してください：

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   編集後、依存関係を取得します：

   ```shell
   dart pub get
   ```

5. **最初のDartコードを書く**：
   
   `lib/` ディレクトリ内に新しいDartファイル、`main.dart` を作成し、シンプルなDartコードを追加します：

   ```dart
   // Dartコアライブラリをインポートする
   import 'dart:core';

   void main() {
     print('Hello, Dart!');
   }
   ```

6. **Dartアプリケーションを実行する**：

   以下でDartプログラムを実行します：

   ```shell
   dart run
   ```

   出力は以下の通りです：

   ```
   Hello, Dart!
   ```

これらの手順に従って、Dartのインストールから最初のDartコードを実行するまで、新しいDartプロジェクトを成功させました。この基礎知識は、スケーラブルなアプリケーションの構築に対してDartの豊かなエコシステムとその機能を深く掘り下げるための土台となります。
