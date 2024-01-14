---
title:                "Go: YAML로 작업하기"
simple_title:         "YAML로 작업하기"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜

YAML을 사용해야 하는 이유는 간단합니다. 이것은 Go 언어로 작성된 매우 인기있는 데이터 시리얼라이제이션 형식입니다. Go로 개발하는 사람들에게는 YAML을 이해하고 사용하는 것이 매우 중요합니다.

## 사용 방법

YAML을 사용하는 것은 매우 간단합니다. 먼저 다음과 같은 Go 패키지를 가져와야 합니다.

```Go
import "gopkg.in/yaml.v2"
```

YAML을 읽을 때는 다음과 같은 코드를 사용할 수 있습니다.

```Go
var data map[string]interface{}
err := yaml.Unmarshal([]byte(yamlString), &data)
if err != nil {
    log.Fatal(err)
}
```

YAML을 쓸 때는 다음과 같은 코드를 사용할 수 있습니다.

```Go
data := map[string]interface{}{
    "name": "John Smith",
    "age": 30,
}
yamlBytes, err := yaml.Marshal(data)
if err != nil {
    log.Fatal(err)
}
fmt.Print(string(yamlBytes))
```

위 코드의 실행 결과는 다음과 같습니다.

```yaml
name: John Smith
age: 30
```

더 많은 예제와 출력 값을 알고 싶다면 [공식 문서](https://pkg.go.dev/gopkg.in/yaml.v2)를 확인하세요.

## 깊게 파고들기

YAML은 데이터를 저장하는 데 유용한 형식입니다. Go로 작업할 때 자주 사용되는 형식이기도 합니다. YAML을 사용할 때 유의해야 할 몇 가지 중요한 점이 있습니다.

첫째, YAML 파일의 구조는 들여쓰기를 기반으로 합니다. 따라서 들여쓰기를 정확하게 지켜야 합니다. 또한 YAML 파일에는 사용할 수 있는 다양한 데이터 유형이 있으므로 [공식 문서](https://yaml.org/spec/)를 참조하여 작업할 수 있습니다.

둘째, YAML 파일에는 지시문을 포함할 수도 있습니다. 예를 들어, 파일의 인코딩 유형을 지정할 수 있습니다. 이러한 지시문은 파일의 맨 위에 작성되며 다음과 같은 형식을 따릅니다.

```yaml
%YAML 1.2
```

더 많은 유용한 정보를 얻고 싶다면 [Go 블로그](https://go.dev/blog/)에서 YAML 관련 게시물을 확인하세요.

## 또 다른 자료

[YAML 공식 문서](https://yaml.org/spec/)

[Go언어와 YAML 사용하기](https://medium.com/eureka-engineering/how-to-use-yaml-in-golang-eef83ep0d8ab)

[Go 언어로 YAML 처리하기](https://flaviocopes.com/go-yaml/)