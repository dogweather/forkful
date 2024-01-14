---
title:    "Arduino: 임시 파일 생성하기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜
아두이노 프로그래밍을 할 때 가끔은 일시적으로 파일을 생성해야 할 때가 있을 수 있습니다. 예를 들어 센서 데이터를 저장하거나 특정 작업을 수행할 때 필요할 수 있습니다.

## 만들어보자
아두이노에서 일시적인 파일을 생성하는 방법은 매우 간단합니다. 먼저 파일을 저장할 변수를 선언한 다음 `createTempFile()` 함수를 사용하여 파일을 생성하고 해당 변수에 데이터를 저장합니다. 이후에는 `print()` 함수를 사용하여 파일에 내용을 출력하고 마지막으로 `close()` 함수를 사용하여 파일을 닫아줍니다.

```Arduino
File tempFile; // 파일을 저장할 변수 선언
tempFile = createTempFile(); // 일시적인 파일 생성
tempFile.print("Hello world!"); // 파일에 내용 출력
tempFile.close(); // 파일 닫기
```

## 깊이 살펴보기
일시적인 파일을 생성하는 방법에 대해 더 자세히 알아보겠습니다. 일시적인 파일은 RAM 메모리에 저장되므로 항상 일정한 용량만을 가집니다. 그렇기 때문에 파일 크기는 매우 작아야 하며 지나친 데이터 저장은 메모리 오버플로우로 이어질 수 있습니다. 또한 일시적인 파일은 재부팅하면 자동으로 삭제되기 때문에 장기적으로 데이터를 저장할 경우에는 영구적인 파일 시스템을 활용해야 합니다.

## 더 알아보기
- [아두이노 공식 홈페이지](https://www.arduino.cc/)
- [아두이노 포럼 (한국어)](https://forum.arduino.cc/index.php?board=7.0)
- [아두이노 예제 코드 및 라이브러리](https://www.arduino.cc/reference/en/)
- [아두이노 저장소](https://github.com/arduino/Arduino)