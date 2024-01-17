---
title:                "임시 파일 생성하기"
html_title:           "Arduino: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

임시 파일을 생성하는 것은 프로그래머들이 자주 하는 일입니다. 이는 일시적으로 사용할 파일을 만들기 위해서입니다. 임시 파일은 작업을 마치면 자동적으로 삭제되므로 시스템에 불필요한 파일이 남지 않습니다.

## 하는 방법:

```Arduino
void createTempFile() {
  File tempFile = SD.open("temp.txt", FILE_WRITE);
  tempFile.println("This is a temporary file.");
  tempFile.close();
}
```

위의 예제는 SD 카드에 "temp.txt"라는 임시 파일을 만들고, 그 안에 "This is a temporary file."라는 내용을 쓰는 코드입니다. 이렇게 생성된 임시 파일은 다른 코드에서 사용하기 위해서 읽거나 쓸 수 있습니다.

## 깊이 파고들기:

상대적으로 오래된 기기인 아두이노에서는 SD 카드와 같은 외부 저장 장치를 다루는데 제한이 있습니다. 따라서 매번 새로운 파일을 생성할 때마다 원래의 파일을 삭제하고 새로 생성해야 합니다.

하지만 최신 버전의 아두이노는 기존 파일을 삭제하지 않고도 임시 파일을 생성할 수 있게 되었습니다. 이는 보다 간편하고 빠르게 작업을 할 수 있게 해줍니다.

대안으로는 도구인 "temFile"가 있습니다. 이는 임시 파일을 생성하고 관리하는데 도움을 주는 라이브러리입니다. 하지만 최신 버전의 아두이노에서는 기본적으로 제공하는 기능으로 충분하기 때문에 별도의 라이브러리를 사용할 필요가 없습니다.

## 관련 자료:

https://www.arduino.cc/en/Reference/TemFile - Aruino 공식 사이트에서 제공하는 임시 파일 생성 관련 도움말 페이지입니다. 

https://github.com/LowPowerLab/SdFat - SdFat 라이브러리는 외부 저장 장치를 다루는데 유용한 도구입니다. 해당 라이브러리에서도 임시 파일을 생성하고 사용하는 방법을 제공합니다.