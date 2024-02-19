---
aliases:
- /ko/go/removing-quotes-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:29.459889-07:00
description: "Go\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C\
  \ \uC81C\uAC70\uD558\uB294 \uAC83\uC740 \uC8FC\uC5B4\uC9C4 \uBB38\uC790\uC5F4\uC758\
  \ \uC120\uD589 \uBC0F \uD6C4\uD589 \uB530\uC634\uD45C(`\"` \uB610\uB294 `'`)\uB97C\
  \ \uC5C6\uC560\uB294 \uAC83\uACFC \uAD00\uB828\uC774 \uC788\uC2B5\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB294 \uC0AC\uC6A9\uC790 \uC785\uB825\uC744 \uC815\uD654\
  \uD558\uAC70\uB098, \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130\uB97C \uB354 \uD6A8\uACFC\
  \uC801\uC73C\uB85C \uBD84\uC11D\uD558\uAC70\uB098, \uB530\uC634\uD45C\uAC00 \uC5C6\
  \uB294 \uCF58\uD150\uCE20\uB97C \uC694\uAD6C\uD558\uB294 \uCD94\uAC00 \uCC98\uB9AC\
  \uB97C \uC704\uD574 \uBB38\uC790\uC5F4\uC744\u2026"
lastmod: 2024-02-18 23:09:05.459819
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\
  \uAC70\uD558\uB294 \uAC83\uC740 \uC8FC\uC5B4\uC9C4 \uBB38\uC790\uC5F4\uC758 \uC120\
  \uD589 \uBC0F \uD6C4\uD589 \uB530\uC634\uD45C(`\"` \uB610\uB294 `'`)\uB97C \uC5C6\
  \uC560\uB294 \uAC83\uACFC \uAD00\uB828\uC774 \uC788\uC2B5\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB294 \uC0AC\uC6A9\uC790 \uC785\uB825\uC744 \uC815\uD654\uD558\
  \uAC70\uB098, \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130\uB97C \uB354 \uD6A8\uACFC\uC801\
  \uC73C\uB85C \uBD84\uC11D\uD558\uAC70\uB098, \uB530\uC634\uD45C\uAC00 \uC5C6\uB294\
  \ \uCF58\uD150\uCE20\uB97C \uC694\uAD6C\uD558\uB294 \uCD94\uAC00 \uCC98\uB9AC\uB97C\
  \ \uC704\uD574 \uBB38\uC790\uC5F4\uC744\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Go에서 문자열에서 따옴표를 제거하는 것은 주어진 문자열의 선행 및 후행 따옴표(`"` 또는 `'`)를 없애는 것과 관련이 있습니다. 프로그래머는 사용자 입력을 정화하거나, 텍스트 데이터를 더 효과적으로 분석하거나, 따옴표가 없는 콘텐츠를 요구하는 추가 처리를 위해 문자열을 준비하기 위해 이 작업을 자주 수행해야 합니다.

## 방법:

문자열에서 따옴표를 제거하기 위한 여러 접근 방법을 Go가 제공하지만, 가장 간단한 방법 중 하나는 `strings` 패키지에서 제공하는 `Trim` 및 `TrimFunc` 함수를 사용하는 것입니다. 방법은 다음과 같습니다:

```go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	quotedString := `"This is a 'quoted' string"`

	// strings.Trim을 사용하여 특정 따옴표 제거
	unquoted := strings.Trim(quotedString, `"'`)
	fmt.Println("strings.Trim 사용:", unquoted)

	// 더 많은 제어를 위해 strings.TrimFunc을 사용한 사용자 정의 접근 방법
	unquotedFunc := strings.TrimFunc(quotedString, func(r rune) bool {
		return r == '"' || r == '\''
	})
	fmt.Println("strings.TrimFunc 사용:", unquotedFunc)
}
```

이 예시는 이중 따옴표(`"`)와 단일 따옴표(`'`)를 모두 제거하는 두 가지 접근 방법을 보여줍니다. `strings.Trim` 함수는 제거해야 할 문자를 정확히 알고 있을 때 더 간단하고 잘 작동합니다. 반면, `strings.TrimFunc`은 제거할 문자를 결정하기 위해 사용자 정의 함수를 지정할 수 있는 더 많은 유연성을 제공합니다. 위 코드의 샘플 출력은 다음과 같습니다:

```
strings.Trim 사용: This is a 'quoted' string
strings.TrimFunc 사용: This is a 'quoted' string
```

두 방법 모두 문자열에서 선행 및 후행 따옴표를 효과적으로 제거합니다.

## 깊은 탐구

`strings` 패키지의 `Trim` 및 `TrimFunc` 함수는 Go의 방대한 표준 라이브러리의 일부로, 제3자 패키지가 필요 없는 강력하면서도 간단한 문자열 조작 기능을 제공하도록 설계되었습니다. 문자열을 효율적으로 처리하고 조작할 필요성은 Go의 네트워크 서버와 데이터 파서에 대한 주요 초점에서 비롯되며, 여기서 문자열 처리는 일반적인 작업입니다.

이들 함수의 주목할 만한 측면 중 하나는 Go의 유니코드 코드 포인트 표현인 룬(runes)을 기반으로 한 구현입니다. 이 설계는 여러 바이트 문자를 포함하는 문자열을 원활하게 처리할 수 있게 해 주어 Go의 문자열 조작 접근 방식을 강력하고 유니코드 친화적으로 만듭니다.

따옴표를 제거하기 위한 `Trim` 및 `TrimFunc`의 직접 사용은 Go에서 편리하고 관용적이지만, 더 복잡한 문자열 처리 작업(예: 중첩된 따옴표, 이스케이프된 따옴표 등)의 경우 정규 표현식(`regexp` 패키지를 통해)이나 수동 파싱이 더 나은 솔루션을 제공할 수 있음을 언급할 가치가 있습니다. 그러나, 이러한 대안들은 더 많은 복잡성과 성능 고려사항을 가져옵니다. 따라서, 단순한 따옴표 제거의 경우, 보여준 방법들은 단순성, 성능 및 기능성 사이에 좋은 균형을 이룹니다.
