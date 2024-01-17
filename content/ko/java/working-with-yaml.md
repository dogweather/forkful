---
title:                "yaml 작업하기"
html_title:           "Java: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

YAML 작업이란 무엇인가요? 이것은 데이터 직렬화 언어로서, 여러분의 프로그램에서 사용하는 구성 설정, 파일 저장 및 데이터 전송을 위해 사용됩니다. 프로그래머는 YAML을 사용하여 일반적으로 사용되는 텍스트 형식보다 더 간결하게 데이터를 표현할 수 있으며, 가독성이 뛰어나다는 이유로 이를 선호합니다.

## 사용 방법:

```Java 
// YAML 라이브러리 가져오기
import org.yaml.snakeyaml.Yaml;

// YAML 파일로부터 데이터 읽기
Yaml yaml = new Yaml();
Map<String, Object> data = yaml.load(inputStream);

// 데이터 조작
data.put("key", "value");

// 변경 사항을 YAML 파일로 다시 작성하기
Yaml yaml = new Yaml();
String output = yaml.dump(data);
```
출력:
```
key: value
```

## 깊게 파헤치기:

### 역사적 배경:
YAML은 2001년 더그 커빈(Doug Cutting)이 만든 고급 마크업 언어인 XML에 대한 대안으로 처음 소개되었습니다. 그동안 여러 가지 다른 데이터 표현 언어가 소개되었지만, YAML은 가장 인기 있는 선택지 중 하나로 자리 잡았습니다.

### 대안:
XML만큼 널리 사용되며 강력한 다른 대안으로는 JSON이 있습니다. 하지만 YAML은 가독성이 좋은 구조와 다른 표현 형식과의 쉬운 호환성 등 여러 가지 장점을 가지고 있어 여전히 많은 프로그램에서 사용됩니다.

### 구현 세부사항:
YAML은 데이터 직렬화 언어이기 때문에, Java에서 다른 직렬화 라이브러리와 비슷한 방식으로 작동합니다. 자세한 내용은 자바 레퍼런스 문서를 참조하시기 바랍니다.

## 관련 자료:

- YAML 공식 홈페이지: https://yaml.org
- YAML 스펙 문서: https://yaml.org/spec/
- SnakeYAML 라이브러리: https://bitbucket.org/asomov/snakeyaml/src/default/