---
title:                "yaml로 작업하기"
html_title:           "Kotlin: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## YAML이란?
YAML은 "YAML Ain't Markup Language"의 약자로, 데이터를 구조화하기 위해 사용하는 경량 마크업 언어입니다. 프로그래머들은 YAML을 사용하여 데이터를 구성하고, 저장하고, 전달하는 데 사용합니다.

## 사용 방법:

### 데이터 구조화하기
```Kotlin
val data = """ 
name: John 
age: 25 
address: 
    street: Main Street 
    city: Anytown 
    country: USA """
```

### 데이터 저장하기
```Kotlin
val writer = File("data.yml").bufferedWriter()
writer.write(data)
writer.close()
```

### 데이터 전달하기
```Kotlin
val inputStream = File("data.yml").inputStream()
val data = inputStream.bufferedReader().readText()
```

### 데이터 파싱하기
```Kotlin
val yaml = Yaml()
val data = yaml.loadAs(inputStream, Map::class.java)
println(data["name"]) // 출력 결과: John
```

## 깊게 파보기:
YAML은 2001년에 처음 소개되었으며, XML과 같은 다른 마크업 언어의 단점을 보완하고자 만들어졌습니다. 다른 대안으로는 JSON이 있습니다. YAML은 JSON보다 읽기 쉽고 이해하기 쉽기 때문에 많은 개발자들이 선호합니다. YAML 파일은 인간이 읽고 수정하기 쉽게 들여쓰기와 들여쓰기 및 특정 형식을 사용하여 표현됩니다.

## 관련 자료:
- [YAML 공식 웹사이트](https://yaml.org/)
- [Kotlin에서 YAML 사용하기](https://www.baeldung.com/kotlin/yaml)
- [다른 마크업 언어 비교 - JSON vs YAML](https://medium.com/@richirufinoygur/marshalling-in-kotlin-with-jackson-leon-yaml-859a235db273)