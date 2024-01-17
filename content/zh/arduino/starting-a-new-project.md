---
title:                "开始新项目"
html_title:           "Arduino: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

# 新項目如何？

## 什麼是新項目？
開始一個新的項目是指在Arduino上創建或開發新的程式或應用程序。程式設計師做這樣的事情是為了滿足新的需求，解決問題，或創造新的產品或服務。

## 為什麼程式設計師會做這件事？
程式設計師通常會開始新的項目，因為現有的程式或應用程序已不再滿足新的需求或要求。另外，開始新的項目也可以讓程式設計師學習新的技能，挑戰自己的能力，並提升自己的知識水平。

## 如何：
在Arduino環境中，創建新的項目非常容易。只需按照以下步驟進行操作：

1. 打開Arduino IDE（集成開發環境）。
2. 點擊「檔案」>「新建」，以創建新的項目。
3. 在程式碼編輯區域，輸入你的程式碼。
4. 按下「上傳」按鈕，將程式碼上傳到Arduino板子。
5. 觀察板子的反應，確保程式成功運行。

以下是一個簡單的程式碼範例，讓LED燈在Arduino板子上閃爍：

```Arduino
// 設定LED引腳
int ledPin = 13;

// 設定程式起始
void setup() {
  // 將LED引腳設定為輸出
  pinMode(ledPin, OUTPUT);  
}

// 主程式
void loop() {
  // 開啟LED燈
  digitalWrite(ledPin, HIGH);   
  // 稍等1秒
  delay(1000);                  
  // 關閉LED燈
  digitalWrite(ledPin, LOW);    
  // 稍等1秒
  delay(1000); 
}
```

該程式會讓板子上的13號引腳（即內建LED燈）以每秒一次的頻率閃爍。

## 深入探討：
### 歷史背景：
Arduino起源於2005年，由義大利的費德里科·穆西奧（Federico Musto）和大衛·卡魯希（David Cuartielles）共同開發。它的目標是提供一個易於學習和使用的平台，讓使用者可以輕鬆地創建電子設備和應用程序。如今，Arduino已成為最受歡迎的開源硬體平台之一。

### 替代方案：
除了Arduino，還有其他的開源硬體平台可以用來開發電子設備和應用程序，如Raspberry Pi、BeagleBone等。不同的平台有不同的優點和用途，使用者可以根據自己的需求選擇最適合的平台。

### 實現細節：
Arduino程式可以使用C/C++語言編寫，但是因為Arduino提供了許多易於使用的函數庫，程式設計師可以使用更簡單的語法和函數來控制硬體設備。此外，Arduino提供了豐富的社群支持和教學資源，讓初學者也能輕鬆上手。

## 相關資源：
- [Arduino官方網站](https://www.arduino.cc/)
- [Arduino中文社群](https://www.arduino.cc/en/Main/ChineseCommunity)
- [Arduino程式設計入門教學](https://maker.gettingsharp.com/entry-level/soft-hardware/arduino/arduino-basic/)