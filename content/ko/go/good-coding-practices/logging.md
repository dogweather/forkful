---
title:                "로깅"
aliases:
- ko/go/logging.md
date:                  2024-02-03T17:59:37.289978-07:00
model:                 gpt-4-0125-preview
simple_title:         "로깅"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/logging.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

소프트웨어 개발에서 로깅은 프로그램 실행에 대한 정보를 기록하는 과정으로, 그 행위를 추적하고 문제를 진단하기 위해 설계되었습니다. 프로그래머들은 소프트웨어 성능을 모니터링하고, 에러를 디버깅하며, 시스템 보안 및 준수를 보장하기 위해 로깅을 구현하여, 애플리케이션 유지 보수 및 분석을 위한 필수 도구로 만듭니다.

## 방법:

Go에서는 표준 라이브러리 패키지 `log`를 사용하여 로깅을 구현할 수 있습니다. 이 패키지는 표준 출력이나 파일로 쓰기와 같은 간단한 로깅 기능을 제공합니다. 표준 출력으로 로깅하는 기본 예제부터 시작해 보겠습니다.

```go
package main

import (
	"log"
)

func main() {
	log.Println("기본 로그 항목입니다.")
}
```

출력:
```
2009/11/10 23:00:00 기본 로그 항목입니다.
```

`log` 패키지에 의해 로그 항목 앞에 자동으로 추가되는 타임스탬프입니다. 다음으로, 표준 출력 대신 파일로 로깅하는 방법을 살펴보겠습니다:

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("이 로그 항목은 파일로 갑니다.")
}
```

이제, 로깅 형식을 사용자 지정하는 더 고급 사용 사례를 구현해 봅시다. Go는 `log.New()`로 사용자 정의 로거를 생성할 수 있습니다:

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "사용자 정의 로그: ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("이것은 사용자 정의 로그 메시지입니다.")
}
```

출력:
```
사용자 정의 로그: 2009/11/10 23:00:00 main.go:11: 이것은 사용자 정의 로그 메시지입니다.
```

이 예제는 각 로그 메시지 앞에 "사용자 정의 로그: "를 접두어로 붙이고 날짜, 시간, 소스 파일 위치를 포함합니다.

## 심층 분석

Go 표준 라이브러리의 `log` 패키지는 직관적이고 많은 애플리케이션에 충분하지만, 구조화된 로깅, 로그 로테이션, 레벨 기반 로깅과 같은 더 정교한 기능이 부족합니다. `zap` 및 `logrus` 같은 패키지는 이러한 고급 기능을 제공하며 Go 커뮤니티에서 그 성능과 유연성으로 잘 알려져 있습니다.

예를 들어, 구조화된 로깅을 통해 JSON과 같은 구조화된 형식으로 데이터를 로깅할 수 있으며, 이는 로그를 다양한 도구나 서비스에서 분석할 수 있는 현대 클라우드 기반 애플리케이션에 특히 유용합니다. `zap`은 특히 높은 성능과 낮은 할당 오버헤드로 알려져 있어 속도와 효율이 중요한 애플리케이션에 적합합니다.

역사적으로, Go에서의 로깅은 언어가 탄생한 이후로 상당히 발전해 왔습니다. Go의 초기 버전은 `log` 패키지에서 보는 기본 로깅 기능을 제공했습니다. 그러나 언어의 인기가 높아지고 Go로 작성된 애플리케이션의 복잡성이 증가함에 따라, 커뮤니티는 그들의 필요를 충족시키기 위해 더 정교한 로깅 라이브러리를 개발하기 시작했습니다. 오늘날, 표준 `log` 패키지는 단순한 애플리케이션에 여전히 실행 가능한 옵션이지만, 많은 개발자들은 더 복잡한 로깅 요구 사항에 대해 이러한 타사 솔루션을 선택합니다.
