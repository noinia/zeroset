-----------------------------------------------------------------------
-- ipelet for drawing zerosets
-----------------------------------------------------------------------

label = "Approximate Bisector"

about = [[
Draws an Approximate Bisector
]]

function storeSelection(model, filePath)
   local p = model:page()
   if not p:hasSelection() then model.ui:explain("no selection") return end

   local doc = ipe.Document()
   local page = doc[1] -- create basic page with one layer and one view

   for i, obj, sel, lay in p:objects() do
    -- --if not (obj:type() == "mark/fdisk" or obj:type() == "mark/disk") then model.ui:explain("select disks or fdisks for input " .. obj:type()) return end
      if sel == 1 or sel == 2 then
         --      local myObj = obj:matrix() * obj:position()
         page:insert(nil, obj, sel, "alpha")
      end
   end
   doc:append(page)
   doc:save(filePath)
end

function compute(inFilePath, outFilePath)
   local ret=0
   ret=_G.os.execute("zeroset -i " .. inFilePath .. " -o " .. outFilePath)
   if not ret then
      model:warning ("failed to compute the zeroset")
      return
   end
end


function insertOutputFrom(filePath,model)
   -- load the doc
   local doc = assert(ipe.Document(filePath))

   local layout = model.doc:sheets():find("layout")
   local fs = layout.framesize

   -- take everything from the first page and insert it into our
   -- document.
   for i,p in doc:pages() do
      local box = ipe.Rect()
      for i,obj,sel,layer in p:objects() do
         box:add(p:bbox(i))
      end

      local nx = (fs.x - box:width()) / 2
      local ny = (fs.y - box:height()) / 2
      local trans = ipe.Vector(nx, ny) - box:bottomLeft()
      local m = ipe.Translation(trans)
      for j = 1,#p do
         p:transform(j, m)
      end

      local elements={}
      for i,obj,sel,layer in p:objects() do
         elements[#elements+1]=obj
      end

      local group = ipe.Group(elements)
      model:creation("create elements", group)
      break
   end
end

function cleanup(paths)
    -- remove tmp files
    for i,f in ipairs(paths) do
       if ipe.fileExists(f) then
          _G.os.execute("rm " .. f)
       end
    end
end

function run(model)
    -- local inFilePath="/tmp/ipelet-zeroset-in.ipe"
    -- local outFilePath="/tmp/ipelet-zeroset-out.ipe"
   local inFilePath="/tmp/ipelet-zeroset-in.ipe"
   local outFilePath="/tmp/out.ipe"

   storeSelection(model, inFilePath)
   compute(inFilePath,outFilePath)
   insertOutputFrom(outFilePath, model)
   -- cleanup({inFilePath,outFilePath})
end
