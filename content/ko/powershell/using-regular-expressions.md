---
title:                "정규 표현식 사용하기."
html_title:           "PowerShell: 정규 표현식 사용하기."
simple_title:         "정규 표현식 사용하기."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 무엇? & 왜?
정규식(regular expressions)을 사용하는 것이란 무엇인지, 그리고 왜 프로그래머들이 이를 사용하는지 알아보겠습니다.

정규식은 문자열에 대한 특정 패턴을 찾고, 추출 또는 대체하는 것입니다. 이는 쉽게 말하면, 문자열에서 원하는 정보를 찾기 위해 사용하는 것입니다. 프로그래머들은 이것을 사용하여 복잡한 문자열 작업을 자동화하고, 처리 속도를 높이기도 합니다.

# 어떻게 해야 할까?
자, 이제 간단한 예시를 통해 정규식을 어떻게 사용하는지 알아보겠습니다. 각 예시는 `PowerShell ...` 코드 블록 안에 작성되었으며, 출력 결과도 함께 보여드리겠습니다.

첫 번째 예시에서는 정규식을 사용하여 주민등록번호 중 뒤에 6자리를 가려보겠습니다.

```PowerShell
$input = "123456-1234567"
$input -replace "-\d{6}", "-******"
```

이를 실행하면 `123456-******7` 이라는 결과가 출력됩니다. 정규식은 `-` 뒤에 6개의 숫자를 찾아 `_`로 대체해줍니다.

두 번째 예시에서는 이메일 주소에 대해 유효성 검사를 해보겠습니다.

```PowerShell
$emails = @("test@test.com", "notanemail", "123@abc")
$emails | where {$_ -match "^\w+@\w+\.\w+$"}
```

출력 결과는 `test@test.com`과 `123@abc` 두 개의 이메일 주소가 출력됩니다. `where` 메서드는 주어진 조건을 만족하는 요소만을 반환하는데, 이 예시에서는 정규식을 사용하여 유효한 이메일 주소만을 추출하도록 하였습니다.

# 깊이 파고들기
자, 이제 더 깊이 파고들어볼까요? 정규식은 1950년대에 발명되었습니다. 당시 IBM 새틀라이트 프로젝트의 일환으로 시작되었는데, 사용하던 텍스트 편집기에서 문자열을 찾는 기능이 없어서 만들어졌습니다.

하지만 이후, 정규식은 다양한 언어와 프로그램에서 사용되기 시작하였습니다. 요즘에는 다양한 언어에서 정규식을 지원하고 있으며, PowerShell도 이에 속합니다.

하지만 정규식을 사용하지 않고도 문자열을 조작할 수 있는 다양한 방법들이 존재하기 때문에, 항상 정규식을 사용해야 하는 것은 아닙니다. 사용 환경과 요구 사항에 따라 적절한 방법을 선택하는 것이 중요합니다.

마지막으로 정규식은 문자열을 순회하면서 패턴이 일치하는 부분을 찾기 때문에, 처리 속도가 느릴 수 있습니다. 하지만 많은 언어들이 최적화를 통해 이를 보완하고 있으므로, 큰 문제가 되지는 않습니다.

# 더 알아보기
더 많은 정보를 원하신다면, 다음 링크들을 참고해보세요!
- [Microsoft Docs의 PowerShell 정규식 가이드](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [자세한 정규식 문법들을 보여주는 사이트](https://regexr.com/)
- [정규식을 사용한 문자열 처리 예시들](https://towardsdatascience.com/regex-cookbook-most-wanted-regex-aa4dde43a9f1)

이제 정규식에 대해 조금 더 알게 되셨을 것입니다. 바쁜 일상에서도 이를 활용하여 보다 효율적인 문자열 처리를 해보세요!