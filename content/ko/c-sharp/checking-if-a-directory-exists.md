---
title:                "디렉토리의 존재 여부 확인하기"
html_title:           "C#: 디렉토리의 존재 여부 확인하기"
simple_title:         "디렉토리의 존재 여부 확인하기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

폴더가 존재하는지 확인하는 것은 파일을 처리하거나 경로를 설정할 때 유용합니다.

## 사용 방법

```C#
if (Directory.Exists(path))
{
    Console.WriteLine("폴더가 존재합니다.");
}
else
{
    Console.WriteLine("폴더가 존재하지 않습니다.");
}
```

위의 코드는 주어진 경로에 폴더가 있는지 없는지를 확인하고 그에 맞는 메시지를 출력하는 간단한 예제입니다. 만약 폴더가 존재한다면 "폴더가 존재합니다."라는 메시지가 출력되고, 폴더가 존재하지 않는다면 "폴더가 존재하지 않습니다."라는 메시지가 출력됩니다. 이렇게 확인하는 것으로 인해 프로그램이 존재하지 않는 폴더를 참조하지 않아 오류를 방지할 수 있습니다.

## 깊이 들어가기

지정된 경로나 경로 문자열을 사용하여 폴더가 존재하는지 확인하는 가장 간단한 방법은 ```Directory.Exists()``` 메서드를 사용하는 것입니다. 이 메서드는 논리 값인 ```true``` 또는 ```false```를 반환합니다. 만약 작업 중에 파일 시스템이 변경되어서 정보가 유효하지 않은 경우에는 예외가 발생할 수도 있으므로 주의해야 합니다.

## 관련 문서

[Directory.Exists 메서드 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.directory.exists) \
[Directory 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.directory)