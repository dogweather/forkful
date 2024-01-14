---
title:                "Arduino: 텍스트 파일 읽기"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜
인간이 아니라 기계가 사용하는 언어로 프로그래밍한 텍스트 파일을 읽을 때 유용한 정보를 제공합니다.

## 어떻게
텍스트 파일을 읽는 것은 매우 간단합니다. 먼저 `Serial` 라이브러리를 `Arduino`에 추가하고, `Serial.begin(9600)`를 사용하여 시리얼 통신을 시작합니다. 그리고 `Serial.println()`을 사용하여 `Hello world!`와 같이 메시지를 출력할 수 있습니다.

살펴보면 다음과 같이 코드를 작성하고 시리얼 모니터를 열고, 전송 속도를 9600bps로 설정한 후 컴퓨터와 `Arduino`를 연결합니다.
```
Arduino
#include <Serial.h>
Serial.begin(9600);

Serial.println("Hello world!");
```

그러면 다음과 같은 결과를 얻을 수 있습니다.
```
Hello world!
```

이제 `Arduino`에서 텍스트 파일을 읽는 방법을 살펴보겠습니다. 다음과 같이 코드를 작성하고 `open()` 함수로 파일을 열고 `println()` 함수로 파일의 내용을 출력합니다.
```
Arduino
String data;
File myFile;

myFile = SD.open("example.txt", FILE_READ);

while (myFile.available()) {
  data = myFile.println();
  Serial.println(data);
}
```

위 코드를 실행하면 `example.txt` 파일의 내용이 시리얼 모니터에 출력됩니다.

## 깊이 파고들기
텍스트 파일을 읽는 것은 프로그래밍에서 매우 중요합니다. 그렇지만 주의할 점이 있습니다. 텍스트 파일을 읽을 때 파일의 크기와 메모리 공간, 파일의 형식에 따라서 읽을 수 있는 용량에 제한이 있을 수 있습니다. 따라서 프로그래머는 파일을 읽기 전에 파일의 크기와 형식을 확인하는 것이 좋습니다.

## 또 다른 정보
- [SD 라이브러리 사용 설명서](https://www.arduino.cc/en/Reference/SD)
- [아두이노로 SD카드 읽기](https://brain-cafe.tistory.com/206)
- [아두이노로 텍스트 파일 읽기](https://m.blog.naver.com/PostView.nhn?blogId=heaven_mat&logNo=220383554352&proxyReferer=https:%2F%2Fwww.google.com%2F)