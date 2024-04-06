---
date: 2024-01-20 17:34:43.699953-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uACFC\uAC70\uC5D4\
  \ `+` \uC5F0\uC0B0\uC790\uB098 `String.Concat` \uBA54\uC11C\uB4DC\uB85C \uBB38\uC790\
  \uC5F4\uC744 \uD569\uCCE4\uC8E0. \uD06C\uAC8C \uBB38\uC81C\uB420 \uAC74 \uC5C6\uC9C0\
  \uB9CC, \uB9CE\uC740 \uC591\uC758 \uBB38\uC790\uC5F4\uC744 \uD569\uCE60 \uB54C\uB294\
  \ \uC131\uB2A5 \uBB38\uC81C\uAC00 \uBC1C\uC0DD\uD560 \uC218 \uC788\uC5B4\uC694.\
  \ \uAC01 \uC5F0\uC0B0\uB9C8\uB2E4 \uC0C8\uB85C\uC6B4 \uBB38\uC790\uC5F4\uC744 \uC0DD\
  \uC131\uD558\uAE30 \uB54C\uBB38\uC785\uB2C8\uB2E4. \uADF8\uB798\uC11C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.564108-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uACFC\uAC70\uC5D4 `+` \uC5F0\uC0B0\
  \uC790\uB098 `String.Concat` \uBA54\uC11C\uB4DC\uB85C \uBB38\uC790\uC5F4\uC744 \uD569\
  \uCCE4\uC8E0."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## How to: (어떻게 하나요?)
```C#
string first = "안녕";
string second = "하세요!";
string combined = first + second;
Console.WriteLine(combined);  // "안녕하세요!" 출력

// StringBuilder 사용
StringBuilder sb = new StringBuilder();
sb.Append(first);
sb.Append(second);
Console.WriteLine(sb.ToString());  // "안녕하세요!" 출력

// String.Format 사용
string formatted = String.Format("{0}{1}", first, second);
Console.WriteLine(formatted);  // "안녕하세요!" 출력

// 문자열 보간 사용
string interpolated = $"{first}{second}";
Console.WriteLine(interpolated);  // "안녕하세요!" 출력
```

## Deep Dive (심층 분석)
과거엔 `+` 연산자나 `String.Concat` 메서드로 문자열을 합쳤죠. 크게 문제될 건 없지만, 많은 양의 문자열을 합칠 때는 성능 문제가 발생할 수 있어요. 각 연산마다 새로운 문자열을 생성하기 때문입니다. 

그래서 `StringBuilder`가 나왔죠. 이 클래스는 문자열을 재분배하지 않고 추가할 수 있어 연산이 적고 대량의 문자열을 더 빠르게 합칠 수 있어요.

`String.Format`은 형식을 지정해 복잡한 문자열 패턴을 쉽게 만듭니다. 변수를 중괄호로 둘러싼 순서에 따라 넣을 수 있죠. 최근에는 문자열 보간이라는 시스템이 소개되었어요. 이건 `$` 기호를 사용하여 변수를 직접 문자열 안으로 넣는 방법이죠. 코드가 더 직관적이고 간결해졌습니다.

## See Also (참고 자료)
- [MSDN - 문자열 병합](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/strings/how-to-concatenate-multiple-strings)
- [MSDN - StringBuilder 클래스](https://docs.microsoft.com/ko-kr/dotnet/api/system.text.stringbuilder)
- [MSDN - String.Format 메서드](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.format)
- [MSDN - 문자열 보간](https://docs.microsoft.com/ko-kr/dotnet/csharp/language-reference/tokens/interpolated)
