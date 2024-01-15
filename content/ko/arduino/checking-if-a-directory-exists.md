---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Arduino: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 왜

디렉토리가 존재하는지 확인하는 것은 우리가 많은 시간을 절약하는 데 도움이 됩니다. 만약 디렉토리가 존재하지 않는다면, 우리는 그것을 만들어야 할 필요가 있기 때문입니다.

## 방법

우선, 디렉토리가 존재하는지 확인하는 방법을 알아보겠습니다.

```Arduino
if (file.exists("/directory_name")) {
  Serial.println("디렉토리가 존재합니다.");
} else {
  Serial.println("디렉토리가 존재하지 않습니다.");
}
```

위의 코드는 파일 시스템 라이브러리인 "SPIFFS"를 사용하여 디렉토리가 존재하는지 여부를 확인하는 예시입니다. 만약 디렉토리가 존재한다면, "디렉토리가 존재합니다."라는 메시지가, 그렇지 않다면 "디렉토리가 존재하지 않습니다."라는 메시지가 시리얼 모니터에 출력됩니다.

## 깊이있게 알아보기

이제 디렉토리를 확인하는 더 자세한 방법에 대해 알아보겠습니다.

디렉토리가 존재하는지 여부를 확인하기 위해 사용하는 함수는 "exists()"입니다. 이 함수는 boolean 값을 리턴하며, 디렉토리의 존재 여부에 따라 true 또는 false를 리턴합니다.

file.exists() 외에도 디렉토리를 확인할 수 있는 다른 함수들도 있습니다. 예를 들면 "isDirectory()"와 "isFile()" 함수가 있습니다. isDirectory() 함수는 해당 경로가 디렉토리인지 확인하고, isFile() 함수는 해당 경로가 파일인지를 확인합니다.

더 자세한 내용은 공식 Arduino 문서를 참고하시면 됩니다.

## 더 알아보기

이번 글에서는 디렉토리가 존재하는지 여부를 확인하는 방법에 대해 알아보았습니다. 만약 이를 응용하여 디렉토리가 존재하지 않는 경우에 디렉토리를 생성하거나, 이미 존재하는 디렉토리인지 확인하는 등 다양한 활용 방법을 추가로 연구해보시기 바랍니다.

## 더 많은 자료

- Arduino 공식 문서: https://www.arduino.cc/reference/en/language/functions/filesystem/existence/exists/
- SPIFFS 라이브러리 사용 방법: https://randomnerdtutorials.com/esp32-web-server-spiffs-spi-flash-file-system/