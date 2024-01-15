---
title:                "표준 에러에 쓰는 방법"
html_title:           "C#: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

표준 오류에 쓰는 것에 대해 궁금해하는 사람들은 주로 디버깅 과정에서 오류 메시지를 출력하거나, 에러 로그를 작성하거나, 사용자에게 오류에 대해 안내해야 할 때 이용합니다.

## 사용 방법

```C#
// 예제 코드
try
{
   // Error가 발생할 수 있는 코드
   throw new Exception("Error 발생!"); 
}
catch (Exception ex)
{
   // Standard Error에 오류 메시지 출력
   Console.Error.WriteLine(ex.Message); 
}
```

위의 예제 코드에서 `Console.Error.WriteLine`을 이용하면 오류 메시지를 표준 오류에 출력할 수 있습니다. 이렇게 하면 사용자가 남아있는 오류를 쉽게 파악할 수 있으며, 디버깅에도 효과적입니다.

`Console.Error`는 `TextWriter` 클래스의 인스턴스로서 기본적으로 표준 오류에 출력되지만, 이를 사용자가 지정한 파일 또는 다른 디바이스로 변경할 수도 있습니다. 또한, `Console.Error`가 아니더라도 `Console.WriteLine`을 이용하여 표준 오류에 메시지를 보낼 수도 있습니다.

## 자세히 알아보기

표준 오류는 그 이름에서도 알 수 있듯이 일반적으로 프로그램에서 발생하는 오류를 표시하는데 사용됩니다. 이는 사용자에게 다음과 같은 이점을 제공합니다.

1. 오류가 발생했을 때 사용자에게 즉각적으로 알려줄 수 있음
2. 디버깅을 위한 정보를 제공하며, 오류를 식별하고 수정하는데 도움이 됨
3. 오류 메시지에 추가적인 정보를 포함하여 사용자에게 정확한 내용을 전달할 수 있음

따라서, 프로그램을 개발하는 과정에서 표준 오류를 사용하는 것은 매우 중요합니다. 이를 통해 사용자에게 오류를 더 간편하게 파악할 수 있고, 디버깅에도 좋은 결과를 얻을 수 있습니다.

## 더 알아보기

표준 오류가 중요한 역할을 하는 이유를 알아보았는데, 이 외에도 표준 입력과 표준 출력이 갖는 중요성에 대해 궁금하다면 아래의 링크들을 참고하시기 바랍니다.

[표준 입출력에 대한 자세한 설명](https://docs.microsoft.com/ko-kr/dotnet/standard/io/)

[본격적인 프로그래밍을 시작하기 전에 알아야 할 것들](http://blog.naver.com/abcde12345/334832/coreyfisher.com/Things_to_know_before_starting_to_code/#Writing_To_Standard_Error) (한글 번역)

## 관련 자료

[빠르게 배우는 C# 시리즈](http://blog.naver.com/abcde12345/334832/coreyfisher.com/Learn_C#) (한글 번역)

[C# 튜토리얼](https://www.tutorialspoint.com/csharp/index.htm)

[C# 기본 개념 및 문법](https://www.charleskung.com/csharp) (한글 번역)