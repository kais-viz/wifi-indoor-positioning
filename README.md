# Evaluate Techniques for Wifi Locationing

## Overview
Now it's time to begin a new project for a new client. Our client is developing a system to be deployed on large industrial campuses, in shopping malls, et cetera to help people to navigate a complex, unfamiliar interior space without getting lost. While GPS works fairly reliably outdoors, it generally doesn't work indoors, so a different technology is necessary. Our client would like us to investigate the feasibility of using "wifi fingerprinting" to determine a person's location in indoor spaces. Wifi fingerprinting uses the signals from multiple wifi hotspots within the building to determine location, analogously to how GPS uses satellite signals. We have been provided with a large database of wifi fingerprints for a multi-building industrial campus with a location (building, floor, and location ID) associated with each fingerprint. Your job is to evaluate multiple machine learning models to see which produces the best result, enabling us to make a recommendation to the client. If your recommended model is sufficiently accurate, it will be incorporated into a smartphone app for indoor locationing.

This is a deceptively difficult problem to solve. I'm looking forward to seeing what you come up with.

## Resources Required
* [UJIIndoorLoc Data Set](http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc)
* [Paper providing a detailed overview of the dataset and how it was collected](https://s3.amazonaws.com/gbstool/courses/614/docs/UJIIndoorLoc%20-%20A%20New%20Multi-building%20and%20Multi-floor%20Database%20for%20WLAN%20Fingerprint-based%20Indoor%20Localization%20Problems.pdf?AWSAccessKeyId=AKIAJBIZLMJQ2O6DKIAA&Expires=1562749200&Signature=BGLcpLGW6wmZiqBBivSYH5cXHNs%3D)
* [Ten things you need to know about indoor positioning](http://www.directionsmag.com/entry/10-things-you-need-to-know-about-indoor-positioning/324602)
* [Indoor Locationing with Wi-Fi FIngerprinting](http://noahpritt.net/files/Aipr2013-WithPubHeading.pdf)
