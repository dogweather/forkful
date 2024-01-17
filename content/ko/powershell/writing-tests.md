---
title:                "테스트 작성하기"
html_title:           "PowerShell: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
테스트를 작성한다는 것은 어떤 코드가 제대로 작동하는지를 확인하는 과정입니다. 프로그래머들은 이를 통해 코드의 품질을 검증하고 버그를 찾는 등의 여러 가지 이유로 테스트를 작성합니다.

## 하기 위해서:
```PowerShell
Describe "테스트 예제" {
    It "출력이 예상대로 나오는지 확인" {
        $Result = Write-Host "안녕하세요!"
        $Expected = "안녕하세요!"
        $Result.Should().Be($Expected)
    }
}

```
출력:
```
안녕하세요!
```

```PowerShell
Describe "다른 예제" {
    It "리스트에 값이 있는지 확인" {
        $List = "사과", "바나나", "딸기"
        $List | Should Contain "자두"
    }
}

```
출력:
```
Expected list {사과, 바나나, 딸기} to contain "자두", but it didn't.
```

## 깊이 들어가보기:
테스트 작성은 오래된 개념이 아닙니다. 1950년대에 프로그래밍의 일부로서 자연스럽게 발전해 나온 것입니다. 또한 다른 언어나 프레임워크에 비해 보다 쉽고 효율적으로 테스트를 작성할 수 있는 파워셸의 장점이 있습니다. 프로그래머들의 생산성을 높이기 위해서는 테스트 작성이 중요합니다.

## 관련 자료:
- [PowerShell 공식 웹사이트](https://docs.microsoft.com/powershell/)
- [PowerShell 테스트 작성 가이드](https://docs.microsoft.com/powershell/scripting/learn/chapter07/powershell-test)
- [PowerShell 테스트 작성을 위한 도구들](https://github.com/PowerShell/PowerShell/wiki/Testing-your-PowerShell-scripts)