---
aliases:
- /ko/go/finding-the-length-of-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:58.569391-07:00
description: "Go\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uB294\
  \ \uAC83\uC740 \uD574\uB2F9 \uBB38\uC790\uC5F4\uC774 \uD3EC\uD568\uD558\uACE0 \uC788\
  \uB294 \uBB38\uC790\uC758 \uC218\uB97C \uACB0\uC815\uD558\uB294 \uAC83\uC5D0 \uAD00\
  \uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBB38\
  \uC790\uC5F4\uC744 \uD6A8\uC728\uC801\uC73C\uB85C \uC870\uC791\uD558\uAE30 \uC704\
  \uD574, \uC720\uD6A8\uC131 \uAC80\uC0AC, \uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C\
  , \uB610\uB294 \uB2E8\uC21C\uD788 \uC0AC\uC6A9\uC790 \uC785\uB825\uC5D0 \uC81C\uC57D\
  \uC744 \uC801\uC6A9\uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC815\uAE30\
  \uC801\uC73C\uB85C \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:05.465035
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uB294\
  \ \uAC83\uC740 \uD574\uB2F9 \uBB38\uC790\uC5F4\uC774 \uD3EC\uD568\uD558\uACE0 \uC788\
  \uB294 \uBB38\uC790\uC758 \uC218\uB97C \uACB0\uC815\uD558\uB294 \uAC83\uC5D0 \uAD00\
  \uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBB38\
  \uC790\uC5F4\uC744 \uD6A8\uC728\uC801\uC73C\uB85C \uC870\uC791\uD558\uAE30 \uC704\
  \uD574, \uC720\uD6A8\uC131 \uAC80\uC0AC, \uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C\
  , \uB610\uB294 \uB2E8\uC21C\uD788 \uC0AC\uC6A9\uC790 \uC785\uB825\uC5D0 \uC81C\uC57D\
  \uC744 \uC801\uC6A9\uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC815\uAE30\
  \uC801\uC73C\uB85C \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
Go에서 문자열의 길이를 찾는 것은 해당 문자열이 포함하고 있는 문자의 수를 결정하는 것에 관한 것입니다. 프로그래머들은 문자열을 효율적으로 조작하기 위해, 유효성 검사, 부분 문자열 추출, 또는 단순히 사용자 입력에 제약을 적용하기 위해 이 작업을 정기적으로 수행합니다.

## 하는 방법:
Go에서 문자열은 변경 불가능한 바이트 시퀀스로 취급됩니다. 내장된 `len()` 함수를 사용하여 문자열의 길이를 찾을 수 있으며, 이는 바이트 수를 반환하지만, 반드시 문자의 수를 반환하지는 않습니다. 사용 방법은 다음과 같습니다:

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// len()을 사용하여 바이트 길이 찾기
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("Byte Length:", byteLength) // 출력: Byte Length: 13

	// 문자열에서 문자 또는 룬의 정확한 수를 얻기 위해
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("Rune Length:", runeLength) // 출력: Rune Length: 9
}
```
`len()`을 사용한 첫 번째 방법은 바이트를 계산하기 때문에 항상 예상된 결과를 제공하지 않을 수 있습니다. ASCII가 아닌 문자(예: "世界")가 포함된 문자열의 경우, Unicode 코드 포인트를 정확하게 계산하기 위해 `unicode/utf8` 패키지의 `RuneCountInString`을 대신 사용해야 합니다.

## 심층 분석
Go 1 이전에는 바이트 시퀀스 대 문자 시퀀스로 문자열을 처리하는 명확한 구분이 없었습니다. Post Go 1에서 문자열의 표준 인코딩 스키마로 UTF-8을 채택함에 따라 더 명확한 접근 방법이 필요했습니다. `len()` 함수는 문자가 단일 바이트로 표현되는 ASCII 문자열에 완벽하게 작동합니다. 그러나 Go 애플리케이션이 더 글로벌해지고, 다양한 언어와 문자 세트를 지원할 필요성이 커짐에 따라, `len()`의 단순한 접근 방식은 한계를 보였습니다.

`utf8.RuneCountInString()`의 도입 및 사용은 Go 용어로 실제 Unicode 문자(룬)를 계산하는 방법을 제공함으로써 이러한 한계에 대한 해답을 제공합니다. 이 방법은 문자가 여러 바이트에 걸쳐 있을 수 있는 UTF-8의 인코딩 특성과 독립적으로 길이 계산을 보장합니다.

문자열을 룬의 슬라이스로 처리하여, Go의 동시성과 효율성 정신에 더 부합하는 대안적 접근 방법을 갖는 것이 포함될 수 있습니다. 그러나, 이 방법은 변환 단계를 필요로 하며 Unicode의 모든 복잡성(예: 결합 문자)을 즉각적으로 해결하지는 않습니다.

요약하자면, `len()`은 바이트 길이에 적합하고 ASCII 텍스트에 대한 효율적인 선택이지만, `utf8.RuneCountInString()`은 전 세계적으로 호환 가능한 애플리케이션에 더 신뢰할 수 있는 선택입니다. 그러나 개발자들은 이러한 선택이 성능과 메모리 사용에 수반하는 트레이드오프를 이해하는 것이 중요합니다.
