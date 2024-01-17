---
title:                "테스트 작성"
html_title:           "Go: 테스트 작성"
simple_title:         "테스트 작성"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/writing-tests.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
테스트 코드를 작성하는 것은 프로그래머가 프로그램의 버그를 찾고 수정하는 것을 돕기 위해 하는 것입니다. 테스트 코드는 코드의 올바른 작동을 보장하고 코드의 변경 사항이 예상치 못한 버그를 일으키지 않는지 확인하는 데 사용됩니다.

## 어떻게:
Go 언어에서의 테스트 코드 작성 방법은 간단합니다. ```Go test``` 명령어를 사용하여 테스트를 실행할 수 있고, 코드의 함수들을 테스트할 수 있습니다. 다음은 Go 언어를 사용하여 테스트 코드를 작성하는 예시입니다:

```
func Sum(a int, b int) int {
  return a + b
}

func TestSum(t *testing.T) {
  result := Sum(5, 10)
  if result != 15 {
    t.Error("Sum function did not return the expected value")
  }
}
```

위 코드는 ```Sum``` 함수를 테스트하고, 함수의 결과 값이 예상한 값과 일치하지 않으면 에러를 출력합니다.

## 깊이 들어가기:
테스트 코드의 역사적인 배경은 소프트웨어 개발의 핵심적인 부분이 되어 왔습니다. 이전에는 테스트를 수동으로하고, 버그를 수정한 다음 다시 테스트를 반복해야 했지만, 현재의 자동화 된 테스트는 시간과 비용을 절약할 수 있습니다. 또한 다른 언어에서도 테스트 코드를 작성할 수 있지만, Go 언어의 간단한 문법과 내장된 테스트 기능은 테스트 작성을 매우 효율적이고 쉽게 만듭니다.

## 관련 자료:
- [Go 언어 공식 문서](https://golang.org/doc/)
- [Go 언어 공식 GitHub 저장소](https://github.com/golang/go)
- [Go 언어를 사용한 테스트 코드 작성 예제](https://gobyexample.com/testing)