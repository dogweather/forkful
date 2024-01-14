---
title:                "C#: 임시 파일 만들기"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 생성하는 것에 대해 궁금한 독자들을 위해 이 포스트를 작성했습니다. 임시 파일을 사용하기 위해 많은 이유가 있지만 대표적인 이유는 일시적으로 데이터를 저장하고 나중에 삭제할 수 있는 장점이 있기 때문입니다.

## 사용 방법

임시 파일을 생성하는 것은 매우 간단합니다. 단계는 다음과 같습니다.

1. ```System.IO``` 네임스페이스를 이용하여 임시 파일 클래스를 가져옵니다.
2. 생성할 임시 파일의 경로 및 파일 이름을 지정합니다.
3. 임시 파일을 생성합니다.
4. 임시 파일에 데이터를 쓰고 저장합니다.
5. 임시 파일을 삭제합니다.

아래에 예제 코드와 실행 결과를 제공하겠습니다.

```C#
// 임시 파일 생성을 위한 네임스페이스 불러오기
using System.IO;

// 임시 파일 경로 및 파일 이름 지정
string tempFilePath = @"C:\Users\User\Desktop\temp.txt";

// 임시 파일 생성
FileStream tempFile = File.Create(tempFilePath);

// 임시 파일에 데이터 쓰기
byte[] data = { 72, 101, 108, 108, 111 }; // "Hello"를 바이트 배열로 만들어줍니다.
tempFile.Write(data, 0, data.Length);

// 임시 파일 저장 및 삭제
tempFile.Close();
File.Delete(tempFilePath);
```

위의 코드를 실행하면 ```temp.txt``` 라는 임시 파일이 생성되고 그 안에 "Hello"라는 문자열이 저장됩니다. 그리고 코드가 완료되면 임시 파일이 자동으로 삭제됩니다.

## 딥 다이브

임시 파일을 생성하기 위해 사용되는 클래스는 ```System.IO``` 네임스페이스 안에 있는 ```FileStream```입니다. 이 클래스는 파일을 열고, 쓰고, 읽고, 수정하는 데 사용됩니다. 그리고 임시 파일을 생성할 때 사용되는 ```File.Create()``` 메서드는 내부적으로 ```FileStream``` 클래스를 사용합니다.

파일의 경로와 이름을 지정할 때 ```Path.GetTempFileName()``` 메서드를 사용할 수도 있습니다. 이렇게하면 시스템이 임시 파일의 경로와 이름을 만들어줍니다. 이 메서드는 랜덤한 이름을 가진 임시 파일을 생성하고 그 경로를 반환합니다.

임시 파일을 생성할 때 지정된 경로에 이미 파일이 존재하는 경우, 예외가 발생할 수 있습니다. 이를 방지하기 위해 ```FileOptions``` 열거형을 사용하여 파일을 재정의할 수 있습니다.

## 참고

- [C# FileStream Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.filestream?view=net-5.0)
- [C# FileOptions Enum](https://docs.microsoft.com/en-us/dotnet/api/system.io.fileoptions?view=net-5.0)