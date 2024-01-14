---
title:                "C#: 문자열 추출하기"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜: 부분 문자열 추출에 참여하는 이유

부분 문자열 추출은 프로그래밍에서 자주 사용되는 기술 중 하나입니다. 이 기술을 배우는 것은 더 효율적인 코드 작성과 데이터 처리를 위해 매우 중요합니다.

# 추출하는 방법

부분 문자열을 추출하는 데, 우리는 기본적으로 두 가지 주요한 방법을 사용할 수 있습니다. 첫 번째 방법은 .Substring() 함수를 사용하는 것이고, 두 번째 방법은 .Split() 함수를 사용하는 것입니다.

```C#
// .Substring() 함수를 이용한 예시
string sentence = "나는 한국 사람입니다.";
string substring = sentence.Substring(3); // 결과: "국 사람입니다."

// .Split() 함수를 이용한 예시
string sentence = "나는 프로그래머입니다.";
string[] words = sentence.Split(' '); // 결과: ["나는", "프로그래머입니다."]
```

# 심층 분석

.Substring() 함수는 문자열을 추출하는 데 매우 유용하지만, 반드시 문자열의 길이를 설정해야만 가능합니다. 반면, .Split() 함수는 특정 기준으로 문자열을 분리하여 배열로 반환해줍니다. 이 두 가지 방법을 적절히 조합한다면, 매우 다양한 상황에서 문자열 추출을 수행할 수 있습니다.

## 참고 링크

- [.Substring() 함수에 관한 자세한 설명](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.substring)
- [.Split() 함수에 관한 자세한 설명](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.split)