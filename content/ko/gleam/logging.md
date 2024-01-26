---
title:                "로깅"
date:                  2024-01-26T01:06:45.343979-07:00
model:                 gpt-4-1106-preview
simple_title:         "로깅"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/logging.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
로그란 우리 프로그램에서 벌어지는 일을 기록하는 방식입니다. 마치 작은 블랙박스를 가진 것과 같은데요; 문제가 발생하면 (믿으세요, 발생할 겁니다), 로그는 무슨 일이 일어났는지 파악하고 문제를 진단하며 성능을 최적화하는 데 없어서는 안 될 자료입니다.

## 방법:
Gleam에서는 전형적으로 로깅 라이브러리를 가져와 사용합니다—박스를 열자마자 바로 사용할 수 있는 전용 로깅 메커니즘은 없습니다. 가상의 `gleam_logger` 크레이트를 사용한다고 해봅시다. 여기 그것을 통합하는 방법이 있습니다:

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("앱이 시작되고 있습니다!")
  let result = intense_calculation()

  case result {
    Ok(value) -> 
      gleam_logger.debug("계산 성공", value)
    Error(err) -> 
      gleam_logger.error("계산 실패", err)
  }
}
```

로그에 예상되는 출력 내용은 다음과 같습니다:

```
INFO: 앱이 시작되고 있습니다!
DEBUG: 계산 성공 42
ERROR: 계산 실패 이유: 제로로 나누기
```

## 심층 분석
로그의 기술은 프로그래밍 초기부터 있었습니다. 시스템 운영자들은 컴퓨터로부터 말 그대로 로그를 얻었으니 - 모든 것이 원활하게 돌아가는 것을 확인하는 것이었죠. 시간이 지나면서 로깅은 디지털화되어 소프트웨어 개발의 핵심 부분이 되었습니다.

Gleam은 Erlang 생태계를 타깃으로 하는 상대적으로 젊은 언어기 때문에 내장된 로깅 프레임워크는 없지만, 성숙한 Erlang 로깅 기능이나 다른 커뮤니티 제공 라이브러리를 활용할 수 있습니다. 각자는 다른 기능과 타협점을 지니고 있습니다: 어떤 것들은 구조화된 로깅을 제공할 수 있는 반면, 다른 것들은 간단한 텍스트 출력에 더 초점을 맞춥니다.

이제, 로깅 기능을 구현하는 문제: 간단한가요? 첫눈에는 그렇습니다. 하지만 그 뒤편을 들여다보면, 동시성 처리, I/O 병목현상, 로그 회전, 형식 표준화(구조화된 로깅에는 JSON을 생각하세요), 수준 필터링, 그리고 아마도 분산 추적을 다뤄야 하는 상황을 보게 될 것입니다. 게다가, 함수형 패러다임에서는 일반적으로 로깅과 같은 부작용을 예측 가능하고 통제된 방식으로 처리하는 것이 바람직합니다.

## 참조
Gleam과 그 주변 생태계에서 로깅의 주요 내용에 대해 자세히 알아보는 곳은 여기입니다:
- [Erlang의 :logger 문서](http://erlang.org/doc/apps/kernel/logger_chapter.html): Gleam이 Erlang으로 컴파일되기 때문에 이것은 직접 적용됩니다.
- [Gleam의 표준 라이브러리 문서](https://hexdocs.pm/gleam_stdlib/): 로깅 유틸리티가 추가될 수도 있는 최신 업데이트를 확인하세요.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam): 로깅 라이브러리를 포함하여 사용 가능해질 때 업데이트되는 자료 목록을 엄선한 것입니다.