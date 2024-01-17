---
title:                "텍스트 파일 읽기"
html_title:           "Arduino: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 뭘 & 왜?
텍스트 파일을 읽는 것은 무슨 뜻이고 그걸 왜 프로그래머들이 하는 걸까? 텍스트 파일은 우리가 읽고 쓸 수 있는 일반적인 파일 형식입니다. 프로그래머들은 이를 사용하여 필요한 데이터를 저장하고 불러와서 코드를 더 효율적으로 작성할 수 있습니다.

## 어떻게:
```ino
void setup() {
  File file = SD.open("data.txt"); // data.txt 파일 열기
  if (file) { // 파일이 정상적으로 열린 경우
    while (file.available()) { // 파일이 읽을 수 있는 상태인 경우
      Serial.println(file.readString()); // 파일 내용을 읽어서 시리얼 모니터에 출력
    }
    file.close(); // 파일 닫기
  }
}
```
설명: 위 코드는 Arduino에서 SD 카드의 data.txt 파일을 열어서 내용을 읽고 출력하는 예제입니다. 파일을 열 때, 파일의 존재 여부를 확인하고, 파일을 읽을 수 있는 상태인지도 확인한 후에 파일 내용을 읽습니다. 읽은 내용은 Serial 모니터에 출력하고, 마지막으로 파일을 닫습니다.

## 더 들어가기:
- 텍스트 파일을 읽는 방법은 매우 오래된 방법입니다. 컴퓨터의 초기 시절부터 사용되던 방법이라고 할 수 있습니다.
- 다른 방법으로는 데이터베이스를 사용하는 것이 있습니다. 데이터베이스는 많은 양의 데이터를 효율적으로 다루기 위해 개발된 방법입니다.
- 파일을 열고 읽는 것은 블록단위로 이루어집니다. 따라서 파일 내용이 많을수록 읽는 속도가 느려질 수 있습니다.

## 참고 자료:
- [Arduino SD 라이브러리 문서](https://www.arduino.cc/en/Reference/SD)
- [텍스트 파일에 관한 위키백과 문서](https://ko.wikipedia.org/wiki/%ED%85%8D%EC%8A%A4%ED%8A%B8_%ED%8C%8C%EC%9D%BC) 
- [Arduino와 SD 카드 사용하기: 텍스트 파일 읽기](https://www.instructables.com/id/Arduino-SD-card-use/)