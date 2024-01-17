---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "Arduino: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何&なぜ？

ディレクトリが存在するかどうかを確認することは、プログラマーが特定のファイルやフォルダーが存在するかどうかを判断するための重要な手段です。この情報を知ることにより、プログラムの実行中にエラーが発生することを防ぐことができます。

## 方法：

```
Arduinoを使用して、ディレクトリが存在するかどうかを確認する方法は次のとおりです。
```
```
if (SD.exists("/directory_name")) { //ディレクトリが存在するかどうかを確認する
  //ディレクトリが存在する場合の処理
} else {
  //ディレクトリが存在しない場合の処理
}
```
```
NOTE: "directory_name"の部分には実際のディレクトリ名を入力してください。
```

## 詳細な情報：

ディレクトリの存在を確認する機能は、プログラミング言語や環境によって異なる場合があります。また、ディレクトリの存在を確認する代替手段として、ディレクトリのリストを取得してからその中から特定のディレクトリを探す方法もあります。ArduinoではSD.exists ()関数を使用することで、簡単にディレクトリの存在を確認することができます。

## 関連情報：

- [Arduino SD Library Documentation](https://www.arduino.cc/en/Reference/SD)
- [C++ Standard Library: file system utilities](https://en.cppreference.com/w/cpp/filesystem)
- [Python OS Module: check if directory exists](https://www.geeksforgeeks.org/python-os-module-os-path-exists-method/)