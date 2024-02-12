---
title:                "YAML로 작업하기"
date:                  2024-02-03T18:13:49.264038-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML로 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Go에서 YAML을 다루는 것은 사람이 읽기 편한 데이터 직렬화 표준인 YAML (YAML Ain't Markup Language) 파일을 Go 데이터 구조로 파싱하거나 그 반대로 하는 것을 포함합니다. 프로그래머들이 이 작업을 하는 이유는 YAML의 단순함과 가독성을 활용하여 설정 파일, 애플리케이션 설정 또는 다른 언어로 작성된 서비스와 구성 요소 간의 데이터 교환을 위해 이를 사용합니다.

## 방법:

Go에서 YAML을 다루려면, Go의 표준 라이브러리가 YAML을 직접 지원하지 않기 때문에 YAML 파싱과 직렬화를 지원하는 라이브러리를 먼저 가져와야 합니다. 이 목적을 위한 가장 인기있는 라이브러리는 "gopkg.in/yaml.v3"입니다. 시작하는 방법은 다음과 같습니다:

1. **YAML 패키지 설치:**

```bash
go get gopkg.in/yaml.v3
```

2. **YAML을 Go 구조체로 파싱하기:**

먼저, YAML 데이터의 구조와 일치하는 Go의 구조체를 정의합니다.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

type Config struct {
  Database struct {
    User     string `yaml:"user"`
    Password string `yaml:"password"`
  } `yaml:"database"`
}

func main() {
  var config Config
  data := `
database:
  user: admin
  password: secret
`
  err := yaml.Unmarshal([]byte(data), &config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("User: %s\nPassword: %s\n", config.Database.User, config.Database.Password)
}
```

**샘플 출력:**

```
User: admin
Password: secret
```

3. **Go 구조체를 YAML로 직렬화하기:**

Go 구조체를 다시 YAML로 변환하는 방법은 다음과 같습니다.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

func main() {
  config := Config{
    Database: struct {
      User     string `yaml:"user"`
      Password string `yaml:"password"`
    }{
      User:     "admin",
      Password: "supersecret",
    },
  }

  data, err := yaml.Marshal(&config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("---\n%s\n", string(data))
}
```

**샘플 출력:**

```yaml
---
database:
  user: admin
  password: supersecret
```

## 깊은 이해:

소프트웨어 개발에서 YAML의 사용은 가독성 좋은 형식으로 인해 구성 파일, 문서, 데이터 교환 형식을 위한 이상적인 선택으로 성장하였습니다. JSON과 비교할 때, YAML은 주석, 스칼라 타입 및 관계 기능을 제공하여 풍부한 데이터 직렬화 프레임워크를 제공합니다. 하지만, 이러한 유연성과 기능은 파싱의 복잡성을 증가시키며, 주의를 기울이지 않을 경우 잠재적인 보안 위험(예: 임의 코드 실행)을 초래할 수 있습니다.

Go용 "gopkg.in/yaml.v3" 라이브러리는 YAML 처리를 위한 강력한 솔루션으로, 사용의 용이성과 포괄적인 기능 지원 간의 균형을 제공합니다. 현재 상태로, "go-yaml/yaml" ( "gopkg.in/yaml.v3" 뒤에 있는 라이브러리)과 같은 대안들이 있지만, 선택된 버전은 보통 특정 프로젝트 요구 사항이나 개인적인 선호도에 따라 다릅니다. 대규모 데이터 세트나 성능에 중요한 애플리케이션을 다룰 때 개발자들은 파싱 시간과 메모리 오버헤드가 적은 JSON과 같은 더 간단한 형식을 고려할 수 있습니다. 그럼에도 불구하고, 가독성과 사용의 용이성이 최우선되는 구성 파일이나 설정에 있어서 YAML은 Go 생태계에서 강력한 경쟁자로 남아 있습니다.
