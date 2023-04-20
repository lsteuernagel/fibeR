function tdt_export(inputpath,outputpath,sdkpath,id,names,N)
    %adding TDTbin2mat to Matlab
    SDKPATH = sdkpath; % '~/Documents/Matlab/custom_libs/';
    addpath(genpath(SDKPATH));
    % Parses a project folder recorded with TDT DAQ / Synapse and returns a data
    % matrix where columns correspond to channels listed in names.
    % 2019-02-01. Leonardo Molina.
    raw = TDTbin2mat(inputpath);
    frequency = raw.streams.(names{1}).fs;
    time = (1:length(raw.streams.(names{1}).data))/raw.streams.(names{1}).fs;
    %nSamples = min(numel(raw.streams.(names{1}).data),numel(raw.streams.(names{2}).data));
    nChannels = numel(names);
    disp(nChannels)
    % get nSamples
    all_mins = zeros(1,nChannels);
    for c = 1:nChannels
       all_mins(c) = numel(raw.streams.(names{c}).data);
    end
    nSamples = min(all_mins);
    data = NaN(nSamples, nChannels + 1);
    for c = 1:nChannels
        data(:, c + 1) = double(raw.streams.(names{c}).data(1:nSamples));
    end
    data(:, 1) = double(transpose(1:nSamples) / frequency);
    % write to file
    out = strcat(outputpath,id,'_export.txt');
    dlmwrite(out, data, 'delimiter','\t','newline','pc','precision',13);
   % writematrix(data,out,'Delimiter','tab')
   % dlmwrite('/beegfs/scratch/bruening_scratch/lsteuernagel/data/fiberPhotometry/fromCorinna/040820BAU0000587_flat.txt', data, 'delimiter','\t','newline','pc','precision',13);
end

% /beegfs/bin/matlab_2014b -nodisplay -nojvm -r 'tdt_export inputpath outputpath names; exit;'
