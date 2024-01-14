---
title:                "C++: 「Yamlを扱う」"
simple_title:         "「Yamlを扱う」"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜYAMLを使うのか

YAMLは、人間が読みやすく、理解しやすいテキスト形式のデータシリアライゼーション言語です。これは、プログラムやアプリケーション間でデータをやりとりする際に非常に便利です。また、YAMLはXMLやJSONよりもシンプルで、コーディングの手間を減らすことができます。

## YAMLの使い方

まず、YAMLの文法を理解する必要があります。例えば、次のようなYAMLファイルを作成することができます。

```C++
person:
   name: John
   age: 30
   profession: Programmer
```

この例では、personというキーの下にname、age、professionのキーとそれぞれの値が格納されています。このように、YAMLは階層構造を持つことができます。

YAMLファイルを読み込むには、ライブラリを使用する必要があります。C++でYAMLを扱う場合、"yaml-cpp"というライブラリが便利です。次の例では、yaml-cppを使用してpersonデータを読み込み、ageの値を出力する方法を示します。

```C++
#include <iostream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node person = YAML::LoadFile("person.yaml");

    // ageの値を取得し、出力する
    std::cout << person["age"] << std::endl;

    return 0;
}
```

上記の例では、yaml-cppライブラリのLoadFileメソッドを使用して、person.yamlファイルを読み込んでいます。また、YAML::Nodeクラスを使用してpersonデータを扱っています。

## YAMLの詳細

YAMLは、RubyとPythonで開発されたもので、C++でも人気があります。また、YAMLには豊富なデータ型があり、リストやマップ、複数行のテキストなどを格納することができます。さらに、YAMLは設定ファイルのような簡単なデータだけでなく、コンフィギュレーションファイルやメッセージングデータなど、さまざまな用途で使用されています。

## See Also

- [YAML公式サイト](https://yaml.org/)
- [yaml-cppライブラリ](https://github.com/jbeder/yaml-cpp)
- [YAMLの基本的な文法説明](https://www.digitalocean.com/community/tutorials/an-introduction-to-yaml)