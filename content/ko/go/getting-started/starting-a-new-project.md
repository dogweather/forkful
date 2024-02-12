---
title:                "새 프로젝트 시작하기"
aliases: - /ko/go/starting-a-new-project.md
date:                  2024-02-03T18:09:46.276366-07:00
model:                 gpt-4-0125-preview
simple_title:         "새 프로젝트 시작하기"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/starting-a-new-project.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇과 왜?

Go로 새 프로젝트를 시작한다는 것은 워크스페이스를 설정하고 필요한 Go 모듈로 초기화하는 것을 포함합니다. 프로그래머는 이를 통해 코드를 조직화하고, 의존성을 효과적으로 관리하며, 빌드 과정을 용이하게 하기 위해 이러한 작업을 합니다. 이는 Go에서 확장 가능하고 유지보수 가능한 소프트웨어를 만드는 데 기초적인 것입니다.

## 방법:

먼저 터미널에서 `go version`을 실행하여 Go가 설치되어 있는지 확인하세요. 설치된 Go의 버전이 출력되어야 합니다. 다음으로, 새 프로젝트를 시작해 봅시다. 워크스페이스로 이동한 후 다음을 실행하세요:

```shell
mkdir hello-world
cd hello-world
```

이 명령은 프로젝트를 위한 새 디렉토리를 생성하고 그곳으로 이동합니다. 이제 모듈을 초기화하세요:

```shell
go mod init example.com/hello-world
```

`example.com/hello-world`를 여러분의 모듈 경로로 바꾸세요. 이 명령은 디렉토리에 `go.mod` 파일을 생성해 새 Go 모듈의 시작을 알립니다. `go.mod`는 다음과 같이 보일 수 있습니다:

```plaintext
module example.com/hello-world

go 1.18
```

`go.mod`는 프로젝트의 의존성을 추적합니다. 이제 `main.go` 파일을 생성하세요:

```shell
touch main.go
```

선호하는 편집기에서 `main.go`를 열고 "Hello, World!"를 출력하는 다음 코드를 추가하세요:

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

프로그램을 실행하려면 터미널로 다시 돌아가서 실행하세요:

```shell
go run main.go
```

다음을 볼 수 있습니다:

```plaintext
Hello, World!
```

축하합니다! 방금 새 Go 프로젝트를 시작하고 첫 번째 Go 프로그램을 실행했습니다.

## 심층 탐구

의존성 관리를 위한 표준으로 모듈을 도입하는 이니셔티브는 Go 1.11에서 공식적으로 채택되었으며, Go 생태계에서 중요한 변화였습니다. 모듈 이전에는 Go 개발자들이 의존성을 관리하기 위해 GOPATH 환경 변수에 의존했으며, 이는 직관적이지 않고 종종 악명 높은 "의존성 지옥"으로 이어졌습니다.

모듈은 프로젝트 의존성, 버전 관리를 캡슐화된 방식으로 관리하고, Go 프로젝트를 더 자체적이고 휴대 가능하게 만드는 방향으로 나아가게 합니다. 각 모듈은 `go.mod` 파일에서 Go가 추적하는 의존성을 지정합니다. 이는 다양한 환경 및 개발 단계에서 의존성 관리를 단순화합니다.

그러나 Go 모듈이 현재 표준이기는 하지만, 일부 레거시 프로젝트는 여전히 GOPATH를 사용할 수 있습니다. 대부분의 새 프로젝트에는 모듈이 더 간단하고 효과적인 관리 시스템을 제공하지만, 더 오래된 Go 코드베이스를 유지 관리하거나 기여하는 데 있어 GOPATH를 이해하는 것이 유용할 수 있습니다.

대안으로서, Go 모듈은 현재 사실상의 표준이지만, 과거에 Go 커뮤니티는 `dep`과 같은 다른 의존성 관리 도구를 실험해 왔습니다. 그러나 이들은 대부분 Go 도구 체인에 통합된 공식 모듈 지원으로 대체되었습니다.
