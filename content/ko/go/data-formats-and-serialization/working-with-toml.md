---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:56.522896-07:00
description: "\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB098\uC694: Go\uC5D0\uC11C TOML\uB85C\
  \ \uC791\uC5C5\uC744 \uC2DC\uC791\uD558\uB824\uBA74 Go \uD45C\uC900 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uAC00 TOML\uC744 \uAE30\uBCF8\uC801\uC73C\uB85C \uC9C0\uC6D0\uD558\
  \uC9C0 \uC54A\uC73C\uBBC0\uB85C TOML \uD30C\uC77C\uC744 \uAD6C\uBB38 \uBD84\uC11D\
  \uD560 \uC218 \uC788\uB294 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD3EC\uD568\uD574\
  \uC57C \uD569\uB2C8\uB2E4. `BurntSushi/toml` \uD328\uD0A4\uC9C0\uB294 \uC774\uB97C\
  \ \uC704\uD55C \uC778\uAE30 \uC788\uB294 \uC120\uD0DD\uC785\uB2C8\uB2E4. \uBA3C\uC800\
  \ \uC124\uCE58\uB97C\u2026"
lastmod: '2024-03-13T22:44:54.497233-06:00'
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C TOML\uB85C \uC791\uC5C5\uC744 \uC2DC\uC791\uD558\uB824\uBA74\
  \ Go \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 TOML\uC744 \uAE30\uBCF8\uC801\
  \uC73C\uB85C \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC73C\uBBC0\uB85C TOML \uD30C\uC77C\
  \uC744 \uAD6C\uBB38 \uBD84\uC11D\uD560 \uC218 \uC788\uB294 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB97C \uD3EC\uD568\uD574\uC57C \uD569\uB2C8\uB2E4."
title: "TOML\uACFC \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 39
---

## 어떻게 사용하나요:
Go에서 TOML로 작업을 시작하려면 Go 표준 라이브러리가 TOML을 기본적으로 지원하지 않으므로 TOML 파일을 구문 분석할 수 있는 라이브러리를 포함해야 합니다. `BurntSushi/toml` 패키지는 이를 위한 인기 있는 선택입니다. 먼저 설치를 확인하세요:

```bash
go get github.com/BurntSushi/toml
```

다음은 그것을 사용하는 간단한 예입니다. `config.toml`이라는 이름의 설정 파일이 다음과 같은 내용을 가지고 있다고 가정해 보세요:

```toml
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

이제 TOML 구조를 반영하는 Go 구조체를 생성해야 합니다:

```go
package main

import (
    "fmt"
    "github.com/BurntSushi/toml"
)

type Config struct {
    Title    string
    Database Database `toml:"database"`
}

type Database struct {
    Server        string
    Ports         []int
    ConnectionMax int `toml:"connection_max"`
    Enabled       bool
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s\n", config.Title)
    fmt.Printf("Database Server: %s\n", config.Database.Server)
}
```

샘플 출력:

```
Title: TOML Example
Database Server: 192.168.1.1
```

## 심층 탐구
TOML은 GitHub의 공동 창립자 중 한 명인 Tom Preston-Werner이 해시 테이블에 쉽게 매핑할 수 있으며 형식에 대한 사전 지식 없이도 한눈에 이해할 수 있는 직관적인 설정 파일 형식을 제공하기 위해 만들었습니다. 이것은 중괄호, 따옴표, 들여쓰기 문제 때문에 구성 파일에 대해 덜 인간 친화적일 수 있는 JSON이나 YAML과 반대됩니다.

Go에서의 `BurntSushi/toml` 패키지는 단순히 디코딩뿐만 아니라 TOML 파일의 인코딩도 허용하는 강력한 라이브러리로, 이 형식의 구성 파일을 읽고 쓸 필요가 있는 응용 프로그램에 다양하게 적합한 선택입니다. 하지만, 기술의 발전과 새로운 Go 버전의 도입으로 인해 `pelletier/go-toml`과 같은 대안이 등장, 향상된 성능과 트리 조작 및 쿼리 지원과 같은 추가 기능을 제공하게 되었음을 유념해야 합니다.

TOML은 많은 애플리케이션에 좋은 선택이지만, 애플리케이션 구성의 복잡성 및 개인 또는 팀의 선호에 따라, TOML의 장황한 특성이 우아하게 포착하지 못할 수 있는 더 복잡한 데이터 구조가 필요한 경우 YAML이나 JSON과 같은 다른 형식이 더 적합할 수 있습니다. 그럼에도 불구하고, 직관적이고 읽기 쉽고 쉽게 편집할 수 있는 구성을 위해서는, 앞서 언급한 라이브러리와 함께 Go의 강력한 타입 시스템을 사용한 TOML이 탁월한 선택입니다.
