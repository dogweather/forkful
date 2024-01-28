---
title:                "로깅"
date:                  2024-01-26T01:04:30.652201-07:00
model:                 gpt-4-1106-preview
simple_title:         "로깅"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/logging.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
로깅은 앱 내에서의 사건, 상태 및 데이터 흐름에 대한 기록을 유지하는 것에 관한 일입니다. 프로그래머들은 버그 진단, 성능 모니터링, 그리고 앱의 운영 건강을 추적하기 위해 이를 수행합니다 — 이는 소프트웨어 버전의 항공기 블랙박스와 매우 유사합니다.

## 방법:
Go에서는 표준 라이브러리의 `log` 패키지에서부터 `logrus`와 `zap`과 같은 써드파티 라이브러리에 이르기까지 다양한 방식으로 로깅을 처리할 수 있습니다. 내장된 `log` 패키지를 사용한 간단한 예제가 여기 있습니다:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	// 로그 파일 생성
	logFile, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer logFile.Close()

	// 로그 출력을 파일 설정
	log.SetOutput(logFile)

	// 몇 가지 이벤트를 로그
	log.Println("응용 프로그램 시작...")
	// ... 응용 프로그램 로직 여기 ...
	log.Println("응용 프로그램이 성공적으로 종료되었습니다.")
}
```

이 코드를 실행하면 터미널에 아무런 출력도 보이지 않는데, 모든 게 `app.log`로 들어가기 때문입니다. 로그 파일 안에서 찾을 것들의 한 장면은 이렇습니다:

```
2023/01/02 15:04:05 응용 프로그램 시작...
2023/01/02 15:05:01 응용 프로그램이 성공적으로 종료되었습니다.
```

## 심층 분석
프로그래밍에서의 로깅은 최초의 컴퓨터 시절로 거슬러 올라갑니다, 당시 엔지니어들은 말 그대로 (정확히는 나방) 하드웨어에 깔린 버그들을 발견하고 그들을 기록했습니다! 오늘날에 이르러, 로깅은 복잡한 시스템 내부에서 무슨 일이 일어나고 있는지 이해하는 세련된 방법이 되었습니다.

Go의 `log` 패키지는 다소 단순하지만, 기본적인 애플리케이션에는 충분할 수 있습니다. 하지만, 현대의 분산 시스템의 맥락에서, 또는 로그 출력에 대해 더 미묘한 제어가 필요할 때(예를 들어 여러 심각도 수준과 같은), 더 강력한 솔루션을 모색해볼 수도 있습니다.

`logrus`와 `zap`과 같은 써드파티 로깅 라이브러리는 구조화된 로깅을 제공하는데, 이는 JSON과 같은 복잡한 데이터 유형을 로깅할 수 있게 함으로써, 특히 ELK 스택이나 Splunk 같은 로그 관리 시스템과 함께 사용할 때 로그 해석을 쉽게 만들어줍니다.

로깅 전략을 구현할 때, 성능 영향도를 고려하는 것도 필수적입니다. 고성능 로깅 라이브러리는 애플리케이션의 처리량과 대기 시간에 미치는 영향을 줄이도록 최적화되어 있습니다. 예를 들어, `zap`은 실시간 시스템에 중요할 수 있는 빠르고 낮은 할당량 설계를 자랑합니다. 

다양한 라이브러리 외에도, 로깅 형식과 표준들도 주목할 만합니다. 구조화된 로깅 형식인 JSON은 로그 처리 시스템과 함께 사용될 때 엄청나게 강력할 수 있습니다. 반면, 평문 로그는 사람이 읽기 쉽지만 프로그래밍적으로 파싱하기는 더 어렵습니다.

## 더 보기
Go의 로깅 기능에 대해 더 깊이 탐구하려면, 이러한 자료가 유용할 수 있습니다:

- Go 블로그의 로깅에 관한 글: https://blog.golang.org/logging
- Go를 위한 구조화된 로거 `logrus`: https://github.com/sirupsen/logrus
- 빠르고 구조화되고 레벨이 있는 로거 `zap`: https://github.com/uber-go/zap
- 로그 분석을 위한 ELK 스택(Elasticsearch, Logstash, Kibana): https://www.elastic.co/what-is/elk-stack
- Go 로깅 라이브러리 비교: https://www.loggly.com/blog/benchmarking-5-popular-golang-logging-libraries/
