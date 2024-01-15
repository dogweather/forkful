---
title:                "解析HTML"
html_title:           "Arduino: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/parsing-html.md"
---

{{< edit_this_page >}}

# 为什么： #

你有没有想过如何从网页上获取特定信息？这就是解析HTML的作用。通过解析HTML，你可以从网页中提取需要的数据，例如温度、湿度等等。这对于利用网页数据来控制你的Arduino项目非常有用！

# 如何操作： #

``` Arduino
#include <ESP8266WiFi.h> 
#include <ESP8266HTTPClient.h>

void setup() {

	Serial.begin(115200);
	
	//连接WiFi网络
	WiFi.begin("WiFi名称","WiFi密码");
	
	while (WiFi.status() != WL_CONNECTED) { 
		delay(1000);
		Serial.println("连接中...");
	} 
	Serial.println("连接成功！")

	//建立HTTP请求
	HTTPClient http;

	//发起GET请求，将网页源代码存储到response变量中
	http.begin("https://www.arduino.cc/reference/en/");
	int httpCode = http.GET();
	String response = http.getString();

	//解析网页内容
	int start = response.indexOf("<h1 class=\"bg-dark\">"); //查找<h1 class="bg-dark">标签的起始位置
	int end = response.indexOf("</h1>", start); //查找</h1>标签的结束位置
	String title = response.substring(start + 20, end); //截取<title>标签中的内容并存储到title变量中
	
	//打印结果
	Serial.println(title);
	
	http.end(); //关闭HTTP连接
}

void loop() {
      
}

```

输出结果：The Arduino Language

# 深入了解： #

解析HTML的过程实际上并不复杂。它主要是通过搜索和截取特定标签来获取所需数据。这可以通过使用Arduino的字符串处理功能来完成。值得注意的是，每当HTML结构发生改变时，代码可能就无法正常工作了。因此，建议在编写代码时保持HTML结构稳定。

# 另请参阅： #

- [如何解析HTML](https://medium.com/@gwineel/how-to-parse-html-in-arduino-e0f50c1817a6)
- [Arduino和ESP8266连接WiFi网络](https://randomnerdtutorials.com/how-to-connect-your-arduino-to-wifi/)