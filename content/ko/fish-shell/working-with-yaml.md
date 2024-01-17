---
title:                "yaml로 작업하기"
html_title:           "Fish Shell: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## 뭔데 왜 필요해?  

YAML 작업이 뭐고 프로그래머들이 왜 그렇게 하는지 알아보자.  

YAML은 "YAML Ain't Markup Language"의 약자로, 인간이 읽고 쓰기 쉬운 데이터 직렬화 언어다. 프로그래밍에서 많이 사용되는 데이터 저장 방식 중 하나로, 설정 파일이나 API 요청 파라미터를 정의할 때 자주 사용된다. YAML의 주요 장점은 가독성이 좋고, 다른 언어나 플랫폼 간 호환성이 높다는 것이다.

## 어떻게 할까?

```Fish Shell (최신 버전)```을 사용하면 YAML 형식의 데이터를 빠르고 효율적으로 작업할 수 있다. 아래는 YAML 파일을 읽어오는 예시 코드이다.

```
# YAML 파일 읽어오기
set yaml_file (cat sample.yaml)

# YAML 데이터 출력하기
echo $yaml_file
```

출력 결과:

```
pets:
  - dog
  - cat
  - fish
```

## 깊게 파헤치기

### 역사적 배경

YAML은 2001년에 처음 발표된 인기 있는 데이터 직렬화 언어다. JSON과 비슷한 형식을 가지고 있지만, 인간이 읽고 쓰기에 더 쉬운 형태로 디자인되었다.

### 대안들

YAML은 주로 데이터 직렬화 작업을 위해 많이 사용되지만, 다른 언어나 플랫폼에서는 다른 대안들을 사용하기도 한다. 예를 들어, JavaScript에서는 JSON을, Java에서는 XML을 주로 사용한다.

### 구현 세부사항

Fish Shell에서 YAML을 처리하기 위해서는 ```yq``` 라이브러리를 설치해야 한다. 이 라이브러리는 YAML 파일을 파싱하고 필요한 정보를 추출할 수 있는 도구다.

## 참고 자료

* [YAML 공식 문서](https://yaml.org/)
* [Fish Shell 공식 문서](https://fishshell.com/docs/current/)
* [yq 라이브러리 GitHub 페이지](https://github.com/kislyuk/yq)