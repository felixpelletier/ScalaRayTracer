#!/usr/bin/python

import cloudinary
import cloudinary.uploader
import cloudinary.api

import sys

filename = sys.argv[1] + ".jpg"

cloudinary.uploader.upload(filename,
                           public_id = "ScalaRayTracer/" + filename)
