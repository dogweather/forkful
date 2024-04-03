---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:49.264038-07:00
description: "\uBC29\uBC95: Go\uC5D0\uC11C YAML\uC744 \uB2E4\uB8E8\uB824\uBA74, Go\uC758\
  \ \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 YAML\uC744 \uC9C1\uC811 \uC9C0\
  \uC6D0\uD558\uC9C0 \uC54A\uAE30 \uB54C\uBB38\uC5D0 YAML \uD30C\uC2F1\uACFC \uC9C1\
  \uB82C\uD654\uB97C \uC9C0\uC6D0\uD558\uB294 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uBA3C\uC800 \uAC00\uC838\uC640\uC57C \uD569\uB2C8\uB2E4. \uC774 \uBAA9\uC801\uC744\
  \ \uC704\uD55C \uAC00\uC7A5 \uC778\uAE30\uC788\uB294 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB294 \"gopkg.in/yaml.v3\"\uC785\uB2C8\uB2E4. \uC2DC\uC791\uD558\uB294 \uBC29\uBC95\
  \uC740 \uB2E4\uC74C\uACFC\u2026"
lastmod: '2024-03-13T22:44:54.492127-06:00'
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C YAML\uC744 \uB2E4\uB8E8\uB824\uBA74, Go\uC758 \uD45C\uC900\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 YAML\uC744 \uC9C1\uC811 \uC9C0\uC6D0\uD558\
  \uC9C0 \uC54A\uAE30 \uB54C\uBB38\uC5D0 YAML \uD30C\uC2F1\uACFC \uC9C1\uB82C\uD654\
  \uB97C \uC9C0\uC6D0\uD558\uB294 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uBA3C\uC800\
  \ \uAC00\uC838\uC640\uC57C \uD569\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

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
